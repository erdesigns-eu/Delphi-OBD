"""The teletext Hamming 8/4 table decoding something other than teletext.

Teletext guards every number it sends - the page, the magazine, the row, the
control bits - with a Hamming 8/4 code, and the decoder is a table of two
hundred and fifty-six entries. A wrong table is the worst kind of wrong: it
fails silently, every page simply never arrives, and there is nothing in a
log to say why.

Checking its shape is not enough, and this checker exists because checking
its shape is exactly what was done. A table that puts the four data bits in
the wrong places is still a perfectly good extended Hamming code - sixteen
codewords, each four bits from every other, a hundred and forty-four byte
values readable, a hundred and twelve not - and it will decode a quarter of
a real service's addresses to numbers that are not the ones sent. It passed.
Every teletext page on every channel was lost for it.

So the codewords themselves are checked, against the sixteen the standard
names. Those sixteen are also checked against each other - all different,
none closer than four bits to another - so a typo here is caught too rather
than quietly becoming the new truth.
"""
import os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from common import ROOT

# The Hamming 8/4 codewords, value 0 to 15, as EN 300 706 sends them.
CODEWORDS = [0x15, 0x02, 0x49, 0x5E, 0x64, 0x73, 0x38, 0x2F,
             0xD0, 0xC7, 0x8C, 0x9B, 0xA1, 0xB6, 0xFD, 0xEA]

problems = []


def distance(one, other):
    return bin(one ^ other).count('1')


# The list itself first: everything below is measured against it.
if len(set(CODEWORDS)) != 16:
    problems.append('the sixteen codewords in this checker are not sixteen '
                    'different bytes')
else:
    closest = min(distance(CODEWORDS[i], CODEWORDS[j])
                  for i in range(16) for j in range(i + 1, 16))
    if closest < 4:
        problems.append('two of the codewords in this checker are only %d '
                        'bits apart; an extended Hamming code has none closer '
                        'than four' % closest)

path = os.path.join(ROOT, 'units', 'TeletextCodes.pas')
table = []
if os.path.isfile(path):
    src = open(path, encoding='utf-8', errors='replace').read()
    # The table inside TeletextHamming84, which is the only one there.
    at = src.find('function TeletextHamming84', src.find(
        'function TeletextHamming84') + 10)
    if at < 0:
        problems.append('units/TeletextCodes.pas has no TeletextHamming84 to '
                        'check')
    else:
        opens = src.find('(', src.find('array[0..255] of ShortInt', at))
        body = src[opens:src.find('begin', at)]
        table = [int(n) for n in re.findall(r'-?\d+', body)]
        if len(table) != 256:
            problems.append('the table has %d entries rather than 256'
                            % len(table))
            table = []

if table:
    for value, word in enumerate(CODEWORDS):
        if table[word] != value:
            problems.append('0x%02X should read as %d and reads as %d'
                            % (word, value, table[word]))
    # And one bit wrong anywhere in a codeword still reads as that codeword.
    for value, word in enumerate(CODEWORDS):
        for bit in range(8):
            spoilt = word ^ (1 << bit)
            if table[spoilt] != value:
                problems.append('0x%02X is 0x%02X with one bit wrong and '
                                'should still read as %d; it reads as %d'
                                % (spoilt, word, value, table[spoilt]))
                break
    readable = sum(1 for v in table if v >= 0)
    if readable != 144:
        problems.append('%d of the 256 bytes read as something; a Hamming 8/4 '
                        'table has 144' % readable)

print('=== the teletext Hamming 8/4 table against the codewords ===')
for line in problems:
    print('  %s' % line)
print('  total:', len(problems))
