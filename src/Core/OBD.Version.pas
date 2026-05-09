//------------------------------------------------------------------------------
//  OBD.Version
//
//  Compile-time version constants for the Delphi-OBD package.
//
//  Single source of truth for the package version string. Bumped at every
//  tag (see CHANGELOG.md). Components that need to surface the version
//  read it from here.
//
//  Author      : Ernst Reidinga (ERDesigns)
//  Copyright   : (c) 2026 ERDesigns and Delphi-OBD contributors
//  License     : MIT — see LICENSE
//
//  References  :
//    - https://semver.org/
//
//  History     :
//    2026-05-09  ERD  Initial version, Phase 0 skeleton.
//------------------------------------------------------------------------------

unit OBD.Version;

interface

const
  /// <summary>Major version. Breaking changes increment this.</summary>
  OBD_VERSION_MAJOR = 2;

  /// <summary>Minor version. New features that keep API compatibility
  /// increment this.</summary>
  OBD_VERSION_MINOR = 0;

  /// <summary>Patch version. Bug fixes only increment this.</summary>
  OBD_VERSION_PATCH = 0;

  /// <summary>Pre-release tag. Empty for production builds, otherwise
  /// e.g. <c>'alpha.1'</c>, <c>'beta.2'</c>, <c>'rc.1'</c>.</summary>
  OBD_VERSION_PRERELEASE = 'alpha.0';

  /// <summary>Full SemVer 2.0 version string.</summary>
  /// <remarks>Format: <c>MAJOR.MINOR.PATCH[-PRERELEASE]</c>.</remarks>
  OBD_VERSION = '2.0.0-alpha.0';

  /// <summary>Human-readable copyright line surfaced in About boxes.</summary>
  OBD_COPYRIGHT = '(c) 2026 ERDesigns and Delphi-OBD contributors';

  /// <summary>Project home page URL.</summary>
  OBD_HOMEPAGE = 'https://github.com/erdesigns-eu/Delphi-OBD';

implementation

end.
