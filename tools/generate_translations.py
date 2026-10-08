#!/usr/bin/env python3
"""Generate locale catalogs from en.json through an OpenAI-compatible API."""
import argparse
import json
import os
import re
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
CONFIG = ROOT / 'tools' / 'translation_locales.json'
SOURCE = ROOT / 'translations' / 'en.json'
PLACEHOLDER = re.compile(r'%(?:\d+:)?[-+0-9.*]*[a-zA-Z%]')


def collect(value, path=()):
    """Collect catalog strings under opaque IDs without altering literal JSON keys."""
    result = []
    if isinstance(value, dict):
        for key, item in value.items():
            result.extend(collect(item, path + (key,)))
    elif isinstance(value, str):
        result.append((path, value))
    return result


def expand(values, paths):
    """Rebuild the nested catalog using the original literal JSON key paths."""
    result = {}
    for item_id, value in values.items():
        target = result
        parts = paths[item_id]
        for part in parts[:-1]:
            target = target.setdefault(part, {})
        target[parts[-1]] = value
    return result


def translate_batch(api_url, api_key, model, locale_name, terms, values):
    """Translate one batch while preserving keys, placeholders, and technical terms."""
    prompt = (
        f'Translate the JSON object values from English to {locale_name}. '
        'Return only a JSON object with exactly the same keys. Preserve Delphi '
        'format placeholders, punctuation used for UI spacing, and these technical '
        f'terms verbatim: {", ".join(terms)}. Use concise, natural IPTV editor UI '
        'terminology. Do not translate empty strings, component identifiers, URLs, '
        'protocol values, M3U tags, or product names.\n\n' +
        json.dumps(values, ensure_ascii=False))
    body = json.dumps({
        'model': model,
        'messages': [
            {'role': 'system', 'content': 'You are a professional software localizer.'},
            {'role': 'user', 'content': prompt}
        ],
        'temperature': 0
    }).encode('utf-8')
    request = urllib.request.Request(
        api_url.rstrip('/') + '/chat/completions', data=body,
        headers={'Authorization': f'Bearer {api_key}', 'Content-Type': 'application/json'})
    with urllib.request.urlopen(request, timeout=180) as response:
        payload = json.load(response)
    content = payload['choices'][0]['message']['content'].strip()
    if content.startswith('```'):
        content = re.sub(r'^```(?:json)?\s*|\s*```$', '', content)
    translated = json.loads(content)
    if set(translated) != set(values):
        raise ValueError('Translation response changed the catalog keys')
    for key, source_value in values.items():
        if not isinstance(translated[key], str):
            raise ValueError(f'Translation response returned a non-string for {key}')
        if PLACEHOLDER.findall(source_value) != PLACEHOLDER.findall(translated[key]):
            raise ValueError(f'Translation response changed placeholders for {key}')
        for term in terms:
            if source_value.count(term) != translated[key].count(term):
                raise ValueError(
                    f'Translation response changed protected term {term} for {key}')
    return translated


def main():
    """Generate one or all configured translation catalogs."""
    parser = argparse.ArgumentParser()
    parser.add_argument('--locale', help='Generate only one configured locale')
    parser.add_argument('--batch-size', type=int, default=80)
    parser.add_argument('--model', default=os.getenv('OPENAI_TRANSLATION_MODEL', 'gpt-4.1-mini'))
    parser.add_argument('--api-url', default=os.getenv('OPENAI_BASE_URL', 'https://api.openai.com/v1'))
    parser.add_argument('--overwrite', action='store_true')
    args = parser.parse_args()
    api_key = os.getenv('OPENAI_API_KEY')
    if not api_key:
        parser.error('OPENAI_API_KEY is required')
    config = json.loads(CONFIG.read_text(encoding='utf-8'))
    source = json.loads(SOURCE.read_text(encoding='utf-8-sig'))
    source_items = collect(source)
    paths = {f'item_{index:04d}': path
             for index, (path, _) in enumerate(source_items)}
    source_values = {f'item_{index:04d}': value
                     for index, (_, value) in enumerate(source_items)}
    locales = config['locales']
    if args.locale:
        if args.locale not in locales:
            parser.error(f'Unknown locale: {args.locale}')
        locales = {args.locale: locales[args.locale]}
    for locale, locale_name in locales.items():
        destination = ROOT / 'translations' / f'{locale}.json'
        if destination.exists() and not args.overwrite:
            print(f'{destination.name}: skipped (use --overwrite)')
            continue
        translated = {}
        items = list(source_values.items())
        for offset in range(0, len(items), args.batch_size):
            batch = dict(items[offset:offset + args.batch_size])
            translated.update(translate_batch(
                args.api_url, api_key, args.model, locale_name,
                config['protected_terms'], batch))
            print(f'{destination.name}: {min(offset + args.batch_size, len(items))}/{len(items)}')
        destination.write_text(
            json.dumps(expand(translated, paths), ensure_ascii=False, indent=4) + '\n',
            encoding='utf-8')


if __name__ == '__main__':
    main()
