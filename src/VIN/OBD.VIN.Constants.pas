//------------------------------------------------------------------------------
// UNIT           : OBD.VIN.Constants.pas
// CONTENTS       : OBD VIN Constants
// VERSION        : 1.0
// TARGET         : Embarcadero Delphi 11 or higher
// AUTHOR         : Ernst Reidinga (ERDesigns)
// STATUS         : Open source under Apache 2.0 library
// COMPATIBILITY  : Windows 7, 8/8.1, 10, 11
// RELEASE DATE   : 08/04/2024
//------------------------------------------------------------------------------
unit OBD.VIN.Constants;

interface

uses
  System.SysUtils, System.Generics.Collections,

  OBD.VIN.Types;

//------------------------------------------------------------------------------
// VIN REGIONS
//------------------------------------------------------------------------------
const
  VINRegions: array[0..5] of TVINRegion = (
    (RangeStart: 'A'; RangeEnd: 'C'; Name: 'Africa'),
    (RangeStart: 'J'; RangeEnd: 'R'; Name: 'Asia'),
    (RangeStart: 'S'; RangeEnd: 'Z'; Name: 'Europe'),
    (RangeStart: '1'; RangeEnd: '5'; Name: 'North America'),
    (RangeStart: '6'; RangeEnd: '7'; Name: 'Oceania'),
    (RangeStart: '8'; RangeEnd: '9'; Name: 'South America')
  );

//------------------------------------------------------------------------------
// VIN COUNTRIES
//------------------------------------------------------------------------------
const
  VINCountries: array[0..133] of TVINCountry = (
    (RangeStart: 'AA'; RangeEnd: 'AH'; Name: 'South Africa'; Code: 'ZA'),
    (RangeStart: 'AJ'; RangeEnd: 'AK'; Name: 'Ivory Coast'; Code: 'CI'),
    (RangeStart: 'AL'; RangeEnd: 'AM'; Name: 'Lesotho'; Code: 'LS'),
    (RangeStart: 'AN'; RangeEnd: 'AP'; Name: 'Botswana'; Code: 'BW'),
    (RangeStart: 'AR'; RangeEnd: 'AS'; Name: 'Namibia'; Code: 'NA'),
    (RangeStart: 'AT'; RangeEnd: 'AU'; Name: 'Madagascar'; Code: 'MG'),
    (RangeStart: 'AV'; RangeEnd: 'AW'; Name: 'Mauritius'; Code: 'MU'),
    (RangeStart: 'AX'; RangeEnd: 'AY'; Name: 'Tunisia'; Code: 'TN'),
    (RangeStart: 'AZ'; RangeEnd: 'A1'; Name: 'Cyprus'; Code: 'CY'),
    (RangeStart: 'A2'; RangeEnd: 'A3'; Name: 'Zimbabwe'; Code: 'ZW'),
    (RangeStart: 'A4'; RangeEnd: 'A5'; Name: 'Mozambique'; Code: 'MZ'),
    (RangeStart: 'BA'; RangeEnd: 'BB'; Name: 'Angola'; Code: 'AO'),
    (RangeStart: 'BC'; RangeEnd: 'BC'; Name: 'Ethiopia'; Code: 'ET'),
    (RangeStart: 'BF'; RangeEnd: 'BG'; Name: 'Kenya'; Code: 'KE'),
    (RangeStart: 'BH'; RangeEnd: 'BH'; Name: 'Rwanda'; Code: 'RW'),
    (RangeStart: 'BL'; RangeEnd: 'BL'; Name: 'Nigeria'; Code: 'NG'),
    (RangeStart: 'BR'; RangeEnd: 'BR'; Name: 'Algeria'; Code: 'DZ'),
    (RangeStart: 'BT'; RangeEnd: 'BT'; Name: 'Swaziland'; Code: 'SZ'),
    (RangeStart: 'BU'; RangeEnd: 'BU'; Name: 'Uganda'; Code: 'UG'),
    (RangeStart: 'B3'; RangeEnd: 'B4'; Name: 'Libya'; Code: 'LY'),
    (RangeStart: 'CA'; RangeEnd: 'CB'; Name: 'Egypt'; Code: 'EG'),
    (RangeStart: 'CF'; RangeEnd: 'CG'; Name: 'Morocco'; Code: 'MA'),
    (RangeStart: 'CL'; RangeEnd: 'CM'; Name: 'Zambia'; Code: 'ZM'),
    (RangeStart: 'EA'; RangeEnd: 'E0'; Name: 'Russia'; Code: 'RU'),
    (RangeStart: 'HA'; RangeEnd: 'H0'; Name: 'China'; Code: 'CN'),
    (RangeStart: 'JA'; RangeEnd: 'J0'; Name: 'Japan'; Code: 'JP'),
    (RangeStart: 'KF'; RangeEnd: 'KH'; Name: 'Israel'; Code: 'IL'),
    (RangeStart: 'KL'; RangeEnd: 'KR'; Name: 'South Korea'; Code: 'KR'),
    (RangeStart: 'KS'; RangeEnd: 'KT'; Name: 'Jordan'; Code: 'JO'),
    (RangeStart: 'K1'; RangeEnd: 'K3'; Name: 'South Korea'; Code: 'KR'),
    (RangeStart: 'K5'; RangeEnd: 'K5'; Name: 'Kyrgyzstan'; Code: 'KG'),
    (RangeStart: 'LA'; RangeEnd: 'L0'; Name: 'China'; Code: 'CN'),
    (RangeStart: 'MA'; RangeEnd: 'ME'; Name: 'India'; Code: 'IN'),
    (RangeStart: 'MF'; RangeEnd: 'MK'; Name: 'Indonesia'; Code: 'ID'),
    (RangeStart: 'ML'; RangeEnd: 'MR'; Name: 'Thailand'; Code: 'TH'),
    (RangeStart: 'MS'; RangeEnd: 'MS'; Name: 'Myanmar'; Code: 'MM'),
    (RangeStart: 'MU'; RangeEnd: 'MU'; Name: 'Mongolia'; Code: 'MN'),
    (RangeStart: 'MX'; RangeEnd: 'MX'; Name: 'Kazakhstan'; Code: 'KZ'),
    (RangeStart: 'M1'; RangeEnd: 'M0'; Name: 'India'; Code: 'IN'),
    (RangeStart: 'NA'; RangeEnd: 'NE'; Name: 'Iran'; Code: 'IR'),
    (RangeStart: 'NF'; RangeEnd: 'NG'; Name: 'Pakistan'; Code: 'PK'),
    (RangeStart: 'NJ'; RangeEnd: 'NJ'; Name: 'Iraq'; Code: 'IQ'),
    (RangeStart: 'NL'; RangeEnd: 'NR'; Name: 'Turkey'; Code: 'TR'),
    (RangeStart: 'NS'; RangeEnd: 'NT'; Name: 'Uzbekistan'; Code: 'UZ'),
    (RangeStart: 'NV'; RangeEnd: 'NV'; Name: 'Azerbaijan'; Code: 'AZ'),
    (RangeStart: 'NX'; RangeEnd: 'NX'; Name: 'Tajikistan'; Code: 'TJ'),
    (RangeStart: 'NY'; RangeEnd: 'NY'; Name: 'Armenia'; Code: 'AM'),
    (RangeStart: 'N1'; RangeEnd: 'N5'; Name: 'Iran'; Code: 'IR'),
    (RangeStart: 'N7'; RangeEnd: 'N8'; Name: 'Turkey'; Code: 'TR'),
    (RangeStart: 'PA'; RangeEnd: 'PC'; Name: 'Philippines'; Code: 'PH'),
    (RangeStart: 'PF'; RangeEnd: 'PG'; Name: 'Singapore'; Code: 'SG'),
    (RangeStart: 'PL'; RangeEnd: 'PR'; Name: 'Malaysia'; Code: 'MY'),
    (RangeStart: 'PS'; RangeEnd: 'PT'; Name: 'Bangladesh'; Code: 'BD'),
    (RangeStart: 'P5'; RangeEnd: 'P0'; Name: 'India'; Code: 'IN'),
    (RangeStart: 'RA'; RangeEnd: 'RB'; Name: 'United Arab Emirates'; Code: 'AE'),
    (RangeStart: 'RF'; RangeEnd: 'RK'; Name: 'Taiwan'; Code: 'TW'),
    (RangeStart: 'RL'; RangeEnd: 'RM'; Name: 'Vietnam'; Code: 'VN'),
    (RangeStart: 'RP'; RangeEnd: 'RP'; Name: 'Laos'; Code: 'LA'),
    (RangeStart: 'RS'; RangeEnd: 'RT'; Name: 'Saudi Arabia'; Code: 'SA'),
    (RangeStart: 'RU'; RangeEnd: 'RW'; Name: 'Russia'; Code: 'RU'),
    (RangeStart: 'R1'; RangeEnd: 'R7'; Name: 'Hong Kong'; Code: 'HK'),
    (RangeStart: 'SA'; RangeEnd: 'SM'; Name: 'United Kingdom'; Code: 'GB'),
    (RangeStart: 'SN'; RangeEnd: 'ST'; Name: 'Germany'; Code: 'DE'),
    (RangeStart: 'SU'; RangeEnd: 'SZ'; Name: 'Poland'; Code: 'PL'),
    (RangeStart: 'S1'; RangeEnd: 'S2'; Name: 'Latvia'; Code: 'LV'),
    (RangeStart: 'S3'; RangeEnd: 'S3'; Name: 'Georgia'; Code: 'GE'),
    (RangeStart: 'S4'; RangeEnd: 'S4'; Name: 'Iceland'; Code: 'IS'),
    (RangeStart: 'TA'; RangeEnd: 'TH'; Name: 'Switzerland'; Code: 'CH'),
    (RangeStart: 'TJ'; RangeEnd: 'TP'; Name: 'Czech Republic'; Code: 'CZ'),
    (RangeStart: 'TR'; RangeEnd: 'TV'; Name: 'Hungary'; Code: 'HU'),
    (RangeStart: 'TW'; RangeEnd: 'T1'; Name: 'Portugal'; Code: 'PT'),
    (RangeStart: 'T3'; RangeEnd: 'T5'; Name: 'Republic of Serbia'; Code: 'RS'),
    (RangeStart: 'T6'; RangeEnd: 'T6'; Name: 'Andorra'; Code: 'AD'),
    (RangeStart: 'T7'; RangeEnd: 'T8'; Name: 'Netherlands'; Code: 'NL'),
    (RangeStart: 'UA'; RangeEnd: 'UC'; Name: 'Spain'; Code: 'ES'),
    (RangeStart: 'UH'; RangeEnd: 'UM'; Name: 'Denmark'; Code: 'DK'),
    (RangeStart: 'UN'; RangeEnd: 'UR'; Name: 'Ireland'; Code: 'IE'),
    (RangeStart: 'UU'; RangeEnd: 'UX'; Name: 'Romania'; Code: 'RO'),
    (RangeStart: 'U1'; RangeEnd: 'U2'; Name: 'Macedonia'; Code: 'MK'),
    (RangeStart: 'U5'; RangeEnd: 'U7'; Name: 'Slovakia'; Code: 'SK'),
    (RangeStart: 'U8'; RangeEnd: 'U0'; Name: 'Bosnia & Herzegovina'; Code: 'BA'),
    (RangeStart: 'VA'; RangeEnd: 'VE'; Name: 'Austria'; Code: 'AT'),
    (RangeStart: 'VF'; RangeEnd: 'VR'; Name: 'France'; Code: 'FR'),
    (RangeStart: 'VS'; RangeEnd: 'VW'; Name: 'Spain'; Code: 'ES'),
    (RangeStart: 'VX'; RangeEnd: 'V2'; Name: 'France'; Code: 'FR'),
    (RangeStart: 'V3'; RangeEnd: 'V5'; Name: 'Croatia'; Code: 'HR'),
    (RangeStart: 'V6'; RangeEnd: 'V8'; Name: 'Estonia'; Code: 'EE'),
    (RangeStart: 'WA'; RangeEnd: 'W0'; Name: 'Germany'; Code: 'DE'),
    (RangeStart: 'XA'; RangeEnd: 'XC'; Name: 'Bulgaria'; Code: 'BG'),
    (RangeStart: 'XD'; RangeEnd: 'XE'; Name: 'Russia'; Code: 'RU'),
    (RangeStart: 'XF'; RangeEnd: 'XH'; Name: 'Greece'; Code: 'GR'),
    (RangeStart: 'XJ'; RangeEnd: 'XK'; Name: 'Russia'; Code: 'RU'),
    (RangeStart: 'XL'; RangeEnd: 'XR'; Name: 'Netherlands'; Code: 'NL'),
    (RangeStart: 'XS'; RangeEnd: 'XW'; Name: 'Russia'; Code: 'RU'),
    (RangeStart: 'XX'; RangeEnd: 'XY'; Name: 'Luxembourg'; Code: 'LU'),
    (RangeStart: 'XZ'; RangeEnd: 'X0'; Name: 'Russia'; Code: 'RU'),
    (RangeStart: 'YA'; RangeEnd: 'YE'; Name: 'Belgium'; Code: 'BE'),
    (RangeStart: 'YF'; RangeEnd: 'YK'; Name: 'Finland'; Code: 'FI'),
    (RangeStart: 'YN'; RangeEnd: 'YN'; Name: 'Malta'; Code: 'MT'),
    (RangeStart: 'YS'; RangeEnd: 'YW'; Name: 'Sweden'; Code: 'SE'),
    (RangeStart: 'YX'; RangeEnd: 'Y2'; Name: 'Norway'; Code: 'NO'),
    (RangeStart: 'Y3'; RangeEnd: 'Y5'; Name: 'Belarus'; Code: 'BY'),
    (RangeStart: 'Y6'; RangeEnd: 'Y8'; Name: 'Ukraine'; Code: 'UA'),
    (RangeStart: 'ZA'; RangeEnd: 'ZU'; Name: 'Italy'; Code: 'IT'),
    (RangeStart: 'ZX'; RangeEnd: 'ZZ'; Name: 'Slovenia'; Code: 'SI'),
    (RangeStart: 'Z1'; RangeEnd: 'Z1'; Name: 'San Marino'; Code: 'SM'),
    (RangeStart: 'Z3'; RangeEnd: 'Z5'; Name: 'Lithuania'; Code: 'LT'),
    (RangeStart: 'Z6'; RangeEnd: 'Z0'; Name: 'Russia'; Code: 'RU'),
    (RangeStart: '1A'; RangeEnd: '10'; Name: 'United States'; Code: 'US'),
    (RangeStart: '13'; RangeEnd: '13'; Name: 'United States'; Code: 'US'),
    (RangeStart: '2A'; RangeEnd: '25'; Name: 'Canada'; Code: 'CA'),
    (RangeStart: '3A'; RangeEnd: '3X'; Name: 'Mexico'; Code: 'MX'),
    (RangeStart: '34'; RangeEnd: '34'; Name: 'Nicaragua'; Code: 'NI'),
    (RangeStart: '35'; RangeEnd: '35'; Name: 'Dominican Republic'; Code: 'DO'),
    (RangeStart: '36'; RangeEnd: '36'; Name: 'Honduras'; Code: 'HN'),
    (RangeStart: '37'; RangeEnd: '37'; Name: 'Panama'; Code: 'PA'),
    (RangeStart: '38'; RangeEnd: '39'; Name: 'Puerto Rico'; Code: 'PR'),
    (RangeStart: '4A'; RangeEnd: '40'; Name: 'United States'; Code: 'US'),
    (RangeStart: '5A'; RangeEnd: '50'; Name: 'United States'; Code: 'US'),
    (RangeStart: '5G'; RangeEnd: '5G'; Name: 'United States'; Code: 'US'),
    (RangeStart: '6A'; RangeEnd: '6X'; Name: 'Australia'; Code: 'AU'),
    (RangeStart: '6Y'; RangeEnd: '61'; Name: 'New Zealand'; Code: 'NZ'),
    (RangeStart: '7A'; RangeEnd: '70'; Name: 'United States'; Code: 'US'),
    (RangeStart: '8A'; RangeEnd: '8E'; Name: 'Argentina'; Code: 'AR'),
    (RangeStart: '8F'; RangeEnd: '8G'; Name: 'Chile'; Code: 'CL'),
    (RangeStart: '8L'; RangeEnd: '8N'; Name: 'Ecuador'; Code: 'EC'),
    (RangeStart: '8S'; RangeEnd: '8T'; Name: 'Peru'; Code: 'PE'),
    (RangeStart: '8X'; RangeEnd: '8Z'; Name: 'Venezuela'; Code: 'VE'),
    (RangeStart: '82'; RangeEnd: '82'; Name: 'Bolivia'; Code: 'BO'),
    (RangeStart: '84'; RangeEnd: '84'; Name: 'Costa Rica'; Code: 'CR'),
    (RangeStart: '9A'; RangeEnd: '9E'; Name: 'Brazil'; Code: 'BR'),
    (RangeStart: '9F'; RangeEnd: '9G'; Name: 'Colombia'; Code: 'CO'),
    (RangeStart: '9S'; RangeEnd: '9V'; Name: 'Uruguay'; Code: 'UY'),
    (RangeStart: '91'; RangeEnd: '90'; Name: 'Brazil'; Code: 'BR')
  );

//------------------------------------------------------------------------------
// VIN MANUFACTURERS
//------------------------------------------------------------------------------
const
  VINManufacturers: array[0..553] of TVINManufacturer = (
    (Code: 'AAV'; Name: 'Volkswagen'),
    (Code: 'AC5'; Name: 'Hyundai'),
    (Code: 'ADD'; Name: 'Hyundai'),
    (Code: 'AFA'; Name: 'Ford'),
    (Code: 'AHT'; Name: 'Toyota'),
    (Code: 'JA3'; Name: 'Mitsubishi'),
    (Code: 'JA4'; Name: 'Mitsubishi'),
    (Code: 'JA'; Name: 'Isuzu'),
    (Code: 'JD'; Name: 'Daihatsu'),
    (Code: 'JF'; Name: 'Subaru'),
    (Code: 'JHA'; Name: 'Hino'),
    (Code: 'JHB'; Name: 'Hino'),
    (Code: 'JHC'; Name: 'Hino'),
    (Code: 'JHD'; Name: 'Hino'),
    (Code: 'JHE'; Name: 'Hino'),
    (Code: 'JHF'; Name: 'Honda'),
    (Code: 'JHG'; Name: 'Honda'),
    (Code: 'JHL'; Name: 'Honda'),
    (Code: 'JHM'; Name: 'Honda'),
    (Code: 'JHN'; Name: 'Honda'),
    (Code: 'JHZ'; Name: 'Honda'),
    (Code: 'JH1'; Name: 'Honda'),
    (Code: 'JH2'; Name: 'Honda'),
    (Code: 'JH3'; Name: 'Honda'),
    (Code: 'JH4'; Name: 'Honda'),
    (Code: 'JH5'; Name: 'Honda'),
    (Code: 'JK'; Name: 'Kawasaki'),
    (Code: 'JL5'; Name: 'Mitsubishi'),
    (Code: 'JM1'; Name: 'Mazda'),
    (Code: 'JMB'; Name: 'Mitsubishi'),
    (Code: 'JMY'; Name: 'Mitsubishi'),
    (Code: 'JMZ'; Name: 'Mazda'),
    (Code: 'JN'; Name: 'Infinity'),
    (Code: 'JS'; Name: 'Suzuki'),
    (Code: 'JT3'; Name: 'Toyota'),
    (Code: 'JT'; Name: 'Lexus'),
    (Code: 'JY'; Name: 'Yamaha'),
    (Code: 'KL'; Name: 'Daewoo'),
    (Code: 'KM'; Name: 'Hyundai'),
    (Code: 'KMY'; Name: 'Daelim'),
    (Code: 'KM1'; Name: 'Hyosung'),
    (Code: 'KN'; Name: 'Kia'),
    (Code: 'KNM'; Name: 'Renault'),
    (Code: 'KPA'; Name: 'SsangYong'),
    (Code: 'KPT'; Name: 'SsangYong'),
    (Code: 'LAE'; Name: 'Jinan Qingqi'),
    (Code: 'LAL'; Name: 'Honda'),
    (Code: 'LAN'; Name: 'Changzhou Yamasaki'),
    (Code: 'LBB'; Name: 'Keeway'),
    (Code: 'LBE'; Name: 'Beijing Hyundai'),
    (Code: 'LBM'; Name: 'Zongshen Piaggio'),
    (Code: 'LBP'; Name: 'Yamaha'),
    (Code: 'LB2'; Name: 'Geely'),
    (Code: 'LCE'; Name: 'Hangzhou Chunfeng'),
    (Code: 'LDC'; Name: 'Peugeot'),
    (Code: 'LDD'; Name: 'Dandong'),
    (Code: 'LDF'; Name: 'Dezhou Fulu'),
    (Code: 'LDN'; Name: 'SouEast'),
    (Code: 'LDY'; Name: 'Zhongtong Coach'),
    (Code: 'LET'; Name: 'Jiangling-Isuzu'),
    (Code: 'LE4'; Name: 'Beijing Benz'),
    (Code: 'LFB'; Name: 'FAW'),
    (Code: 'LFG'; Name: 'Taizhou Chuanl '),
    (Code: 'LFP'; Name: 'FAW'),
    (Code: 'LFT'; Name: 'FAW'),
    (Code: 'LFV'; Name: 'FAW'),
    (Code: 'LFW'; Name: 'FAW'),
    (Code: 'LFY'; Name: 'Changshu'),
    (Code: 'LGB'; Name: 'Dong Feng'),
    (Code: 'LGH'; Name: 'Qoros'),
    (Code: 'LGX'; Name: 'BYD'),
    (Code: 'LHB'; Name: 'Beijing Automotive Industry Holding'),
    (Code: 'LH1'; Name: 'FAW'),
    (Code: 'LJC'; Name: 'JAC'),
    (Code: 'LJ1'; Name: 'JAC'),
    (Code: 'LKL'; Name: 'Suzhou King Long'),
    (Code: 'LL6'; Name: 'Hunan Changfeng'),
    (Code: 'LL8'; Name: 'Linhai'),
    (Code: 'LMC'; Name: 'Suzuki'),
    (Code: 'LPR'; Name: 'Yamaha'),
    (Code: 'LPS'; Name: 'Polestar'),
    (Code: 'LRW'; Name: 'Tesla'),
    (Code: 'LSG'; Name: 'General Motors'),
    (Code: 'LSJ'; Name: 'MG'),
    (Code: 'LSV'; Name: 'Volkswagen'),
    (Code: 'LSY'; Name: 'Brilliance Zhonghua'),
    (Code: 'LTP'; Name: 'National Electric Vehicle Sweden AB'),
    (Code: 'LTV'; Name: 'Toyota'),
    (Code: 'LUC'; Name: 'Honda'),
    (Code: 'LVS'; Name: 'Ford'),
    (Code: 'LVV'; Name: 'Chery'),
    (Code: 'LVZ'; Name: 'Dong Feng Sokon Motor Company'),
    (Code: 'LV3'; Name: 'National Electric Vehicle Sweden AB'),
    (Code: 'LZM'; Name: 'MAN'),
    (Code: 'LZE'; Name: 'Isuzu'),
    (Code: 'LZG'; Name: 'Shaanxi'),
    (Code: 'LZP'; Name: 'Baotian'),
    (Code: 'LZY'; Name: 'Yutong Zhengzhou,'),
    (Code: 'LZZ'; Name: 'Chongqing Shuangzing Mech & Elec'),
    (Code: 'L4B'; Name: 'Xingyue Group'),
    (Code: 'L5C'; Name: 'KangDi)'),
    (Code: 'L5K'; Name: 'Zhejiang Yongkang'),
    (Code: 'L5N'; Name: 'Zhejiang Taotao'),
    (Code: 'L5Y'; Name: 'Merato Motorcycle Taizhou Zhongneng'),
    (Code: 'L85'; Name: 'Zhejiang Yongkang Huabao Electric Appliance'),
    (Code: 'L8X'; Name: 'Zhejiang Summit Huawin Motorcycle'),
    (Code: 'MAB'; Name: 'Mahindra & Mahindra'),
    (Code: 'MAC'; Name: 'Mahindra & Mahindra'),
    (Code: 'MAJ'; Name: 'Ford'),
    (Code: 'MAK'; Name: 'Honda'),
    (Code: 'MAL'; Name: 'Hyundai '),
    (Code: 'MAT'; Name: 'Tata Motors'),
    (Code: 'MA1'; Name: 'Mahindra & Mahindra'),
    (Code: 'MA3'; Name: 'Suzuki'),
    (Code: 'MA6'; Name: 'GM'),
    (Code: 'MA7'; Name: 'Mitsubishi'),
    (Code: 'MB8'; Name: 'Suzuki'),
    (Code: 'MBH'; Name: 'Suzuki'),
    (Code: 'MBJ'; Name: 'Toyota'),
    (Code: 'MBR'; Name: 'Mercedes-Benz'),
    (Code: 'MB1'; Name: 'Ashok Leyland'),
    (Code: 'MCA'; Name: 'Fiat'),
    (Code: 'MCB'; Name: 'GM'),
    (Code: 'MC2'; Name: 'Volvo'),
    (Code: 'MDH'; Name: 'Nissan'),
    (Code: 'MD2'; Name: 'Bajaj'),
    (Code: 'MD9'; Name: 'Shuttle Cars'),
    (Code: 'MEC'; Name: 'Daimler'),
    (Code: 'MEE'; Name: 'Renault'),
    (Code: 'MEX'; Name: 'Volkswagen'),
    (Code: 'MHF'; Name: 'Toyota'),
    (Code: 'MHR'; Name: 'Honda'),
    (Code: 'MLC'; Name: 'Suzuki'),
    (Code: 'NAA'; Name: 'Peugeot'),
    (Code: 'NAP'; Name: 'Pars Khodro'),
    (Code: 'MLH'; Name: 'Honda'),
    (Code: 'MMA'; Name: 'Mitsubishi'),
    (Code: 'MMB'; Name: 'Mitsubishi'),
    (Code: 'MMC'; Name: 'Mitsubishi'),
    (Code: 'MMM'; Name: 'Chevrolet'),
    (Code: 'MMS'; Name: 'Suzuki'),
    (Code: 'MMT'; Name: 'Mitsubishi'),
    (Code: 'MMU'; Name: 'Holden'),
    (Code: 'MM8'; Name: 'Mazda'),
    (Code: 'MNB'; Name: 'Ford'),
    (Code: 'MNT'; Name: 'Nissan'),
    (Code: 'MPA'; Name: 'Isuzu'),
    (Code: 'MP1'; Name: 'Isuzu'),
    (Code: 'MRH'; Name: 'Honda'),
    (Code: 'MR0'; Name: 'Toyota'),
    (Code: 'MS0'; Name: 'SSS MOTORS'),
    (Code: 'MS3'; Name: 'Suzuki'),
    (Code: 'NLA'; Name: 'Honda '),
    (Code: 'NLE'; Name: 'Mercedes-Benz'),
    (Code: 'NLH'; Name: 'Hyundai'),
    (Code: 'NLN'; Name: 'Karsan'),
    (Code: 'NLR'; Name: 'OTOKAR'),
    (Code: 'NLT'; Name: 'TEMSA'),
    (Code: 'NMB'; Name: 'Mercedes-Benz'),
    (Code: 'NMC'; Name: 'BMC'),
    (Code: 'NM0'; Name: 'Ford'),
    (Code: 'NM4'; Name: 'Tofaş'),
    (Code: 'NMT'; Name: 'Toyota'),
    (Code: 'NNA'; Name: 'Isuzu'),
    (Code: 'PE1'; Name: 'Ford'),
    (Code: 'PE3'; Name: 'Mazda'),
    (Code: 'PL1'; Name: 'Proton,'),
    (Code: 'PNA'; Name: 'Peugeot'),
    (Code: 'R2P'; Name: 'Evoke'),
    (Code: 'RA1'; Name: 'Steyr'),
    (Code: 'RFB'; Name: 'Kymco'),
    (Code: 'RFG'; Name: 'Sanyang SYM'),
    (Code: 'RFL'; Name: 'Adly'),
    (Code: 'RFT'; Name: 'CPI'),
    (Code: 'RF3'; Name: 'Aeon'),
    (Code: 'SAB'; Name: 'Optare'),
    (Code: 'SAD'; Name: 'Jaguar'),
    (Code: 'SAL'; Name: 'Land Rover'),
    (Code: 'SAJ'; Name: 'Jaguar'),
    (Code: 'SAR'; Name: 'Rover'),
    (Code: 'SAX'; Name: 'Austin-Rover'),
    (Code: 'SA9'; Name: 'OX Global'),
    (Code: 'SB1'; Name: 'Toyota'),
    (Code: 'SBM'; Name: 'McLaren'),
    (Code: 'SCA'; Name: 'Rolls Royce'),
    (Code: 'SCB'; Name: 'Bentley'),
    (Code: 'SCC'; Name: 'Lotus'),
    (Code: 'SCE'; Name: 'DeLorean'),
    (Code: 'SCF'; Name: 'Aston Martin'),
    (Code: 'SCK'; Name: 'iFor Williams'),
    (Code: 'SDB'; Name: 'Peugeot'),
    (Code: 'SED'; Name: 'General Motors'),
    (Code: 'SEY'; Name: 'LDV'),
    (Code: 'SFA'; Name: 'Ford'),
    (Code: 'SFD'; Name: 'Alexander Dennis'),
    (Code: 'SHH'; Name: 'Honda'),
    (Code: 'SHS'; Name: 'Honda'),
    (Code: 'SJN'; Name: 'Nissan'),
    (Code: 'SKF'; Name: 'Vauxhall'),
    (Code: 'SLP'; Name: 'JCB'),
    (Code: 'SMT'; Name: 'Triumph'),
    (Code: 'SUF'; Name: 'Fiat'),
    (Code: 'SUL'; Name: 'FSC'),
    (Code: 'SUP'; Name: 'FSO-Daewoo'),
    (Code: 'SU9'; Name: 'Solaris'),
    (Code: 'SUU'; Name: 'Solaris'),
    (Code: 'SWV'; Name: 'TA-NO '),
    (Code: 'TCC'; Name: 'Smart'),
    (Code: 'TDM'; Name: 'QUANTYA'),
    (Code: 'TK9'; Name: 'SOR'),
    (Code: 'TMA'; Name: 'Hyundai'),
    (Code: 'TMB'; Name: 'Škoda)'),
    (Code: 'TMK'; Name: 'Karosa'),
    (Code: 'TMP'; Name: 'Škoda'),
    (Code: 'TMT'; Name: 'Tatra'),
    (Code: 'TM9'; Name: 'Škoda'),
    (Code: 'TNE'; Name: 'TAZ'),
    (Code: 'TN9'; Name: 'Karosa'),
    (Code: 'TRA'; Name: 'Ikarus'),
    (Code: 'TRU'; Name: 'Audi'),
    (Code: 'TSB'; Name: 'Ikarus'),
    (Code: 'TSE'; Name: 'Ikarus'),
    (Code: 'TSM'; Name: 'Suzuki'),
    (Code: 'TW1'; Name: 'Toyota '),
    (Code: 'TYA'; Name: 'Mitsubishi'),
    (Code: 'TYB'; Name: 'Mitsubishi'),
    (Code: 'UU1'; Name: 'Dacia'),
    (Code: 'UU2'; Name: 'Oltcit'),
    (Code: 'UU3'; Name: 'ARO'),
    (Code: 'UU4'; Name: 'Roman SA'),
    (Code: 'UU5'; Name: 'Rocar'),
    (Code: 'UU6'; Name: 'Daewoo'),
    (Code: 'UU7'; Name: 'Euro Bus Diamond'),
    (Code: 'UU9'; Name: 'Astra'),
    (Code: 'UV9'; Name: 'ATP'),
    (Code: 'UZT'; Name: 'UTB'),
    (Code: 'U5Y'; Name: 'Kia'),
    (Code: 'U6Y'; Name: 'Kia'),
    (Code: 'VAG'; Name: 'Magna Steyr Puch'),
    (Code: 'VAN'; Name: 'MAN'),
    (Code: 'VBK'; Name: 'KTM'),
    (Code: 'VF1'; Name: 'Renault'),
    (Code: 'VF2'; Name: 'Renault'),
    (Code: 'VF3'; Name: 'Peugeot'),
    (Code: 'VF4'; Name: 'Talbot'),
    (Code: 'VF6'; Name: 'Renault'),
    (Code: 'VF7'; Name: 'Citroën'),
    (Code: 'VF8'; Name: 'Matra'),
    (Code: 'VF9'; Name: 'Bugatti'),
    (Code: 'VG5'; Name: 'MBK'),
    (Code: 'VLU'; Name: 'Scania'),
    (Code: 'VN1'; Name: 'SOVAB'),
    (Code: 'VNE'; Name: 'Irisbus'),
    (Code: 'VNK'; Name: 'Toyota'),
    (Code: 'VNV'; Name: 'Renault-Nissan'),
    (Code: 'VSA'; Name: 'Mercedes-Benz'),
    (Code: 'VSE'; Name: 'Suzuki'),
    (Code: 'VSK'; Name: 'Nissan'),
    (Code: 'VSS'; Name: 'SEAT'),
    (Code: 'VSX'; Name: 'Opel'),
    (Code: 'VS6'; Name: 'Ford'),
    (Code: 'VS7'; Name: 'Citroën'),
    (Code: 'VS9'; Name: 'Carrocerias Ayats'),
    (Code: 'VTH'; Name: 'Derbi'),
    (Code: 'VTL'; Name: 'Yamaha'),
    (Code: 'VTT'; Name: 'Suzuki'),
    (Code: 'VV9'; Name: 'TAURO'),
    (Code: 'VWA'; Name: 'Nissan'),
    (Code: 'VWV'; Name: 'Volkswagen'),
    (Code: 'VX1'; Name: 'Zastava / Yugo Serbia'),
    (Code: 'WAG'; Name: 'Neoplan'),
    (Code: 'WAU'; Name: 'Audi'),
    (Code: 'WA1'; Name: 'Audi'),
    (Code: 'WBA'; Name: 'BMW'),
    (Code: 'WBS'; Name: 'BMW'),
    (Code: 'WBW'; Name: 'BMW'),
    (Code: 'WBY'; Name: 'BMW'),
    (Code: 'WB1'; Name: 'BMW'),
    (Code: 'WDA'; Name: 'Daimler'),
    (Code: 'WDB'; Name: 'Mercedes-Benz'),
    (Code: 'WDC'; Name: 'DaimlerChrysler'),
    (Code: 'WDD'; Name: 'Mercedes-Benz'),
    (Code: 'WDF'; Name: 'Mercedes-Benz'),
    (Code: 'WEB'; Name: 'Evobus'),
    (Code: 'WJM'; Name: 'Iveco'),
    (Code: 'WF0'; Name: 'Ford'),
    (Code: 'WKE'; Name: 'Krone'),
    (Code: 'WKK'; Name: 'Kässbohrer/Setra'),
    (Code: 'WMA'; Name: 'MAN'),
    (Code: 'WME'; Name: 'Smart'),
    (Code: 'WMW'; Name: 'MINI'),
    (Code: 'WMX'; Name: 'Mercedes-AMG'),
    (Code: 'WMZ'; Name: 'MINI'),
    (Code: 'WP0'; Name: 'Porsche'),
    (Code: 'WP1'; Name: 'Porsche'),
    (Code: 'WSM'; Name: 'Schmitz-Cargobull'),
    (Code: 'W09'; Name: 'RUF'),
    (Code: 'W0L'; Name: 'Opel'),
    (Code: 'W0V'; Name: 'Opel'),
    (Code: 'W1K'; Name: 'Mercedes'),
    (Code: 'W1V'; name: 'Mercedes'),
    (Code: 'W1N'; Name: 'Mercedes'),
    (Code: 'WAP'; Name: 'BMW Alpine'),
    (Code: 'WUA'; Name: 'Audi'),
    (Code: 'WVG'; Name: 'Volkswagen'),
    (Code: 'WVW'; Name: 'Volkswagen'),
    (Code: 'WV1'; Name: 'Volkswagen'),
    (Code: 'WV2'; Name: 'Volkswagen'),
    (Code: 'WV3'; Name: 'Volkswagen '),
    (Code: 'XLB'; Name: 'Volvo'),
    (Code: 'XLE'; Name: 'Scania'),
    (Code: 'XLR'; Name: 'DAF'),
    (Code: 'XL4'; Name: 'Lightyear'),
    (Code: 'XL9'; Name: 'Spyker'),
    (Code: 'XMC'; Name: 'Mitsubishi'),
    (Code: 'XMG'; Name: 'VDL'),
    (Code: 'XTA'; Name: 'Lada/AvtoVAZ'),
    (Code: 'XTC'; Name: 'KAMAZ'),
    (Code: 'XTH'; Name: 'GAZ'),
    (Code: 'XTT'; Name: 'UAZ/Sollers'),
    (Code: 'XTU'; Name: 'Trolza'),
    (Code: 'XTY'; Name: 'LiAZ'),
    (Code: 'XUF'; Name: 'General Motors'),
    (Code: 'XUU'; Name: 'General Motors'),
    (Code: 'XW8'; Name: 'Volkswagen'),
    (Code: 'XWB'; Name: 'Daewoo'),
    (Code: 'XWE'; Name: 'Hyundai-Kia'),
    (Code: 'X1M'; Name: 'PAZ'),
    (Code: 'X4X'; Name: 'BMW'),
    (Code: 'X7L'; Name: 'Renault'),
    (Code: 'X7M'; Name: 'Hyundai'),
    (Code: 'YAR'; Name: 'Toyota'),
    (Code: 'YBW'; Name: 'Volkswagen'),
    (Code: 'YB1'; Name: 'Volvo'),
    (Code: 'YCM'; Name: 'Mazda'),
    (Code: 'YE2'; Name: 'Van Hool'),
    (Code: 'YH2'; Name: 'Lynx'),
    (Code: 'YK1'; Name: 'Saab-Valmet'),
    (Code: 'YSC'; Name: 'Cadillac'),
    (Code: 'YS2'; Name: 'Scania'),
    (Code: 'YS3'; Name: 'Saab'),
    (Code: 'YS4'; Name: 'Scania'),
    (Code: 'YTN'; Name: 'Saab'),
    (Code: 'YT9'; Name: 'Koenigsegg'),
    (Code: '007'; Name: 'Koenigsegg'),
    (Code: 'YT9'; Name: 'Carvia'),
    (Code: '034'; Name: 'Carvia'),
    (Code: 'YU7'; Name: 'Husaberg'),
    (Code: 'YVV'; Name: 'Polestar'),
    (Code: 'YV1'; Name: 'Volvo'),
    (Code: 'YV4'; Name: 'Volvo'),
    (Code: 'YV2'; Name: 'Volvo'),
    (Code: 'YV3'; Name: 'Volvo'),
    (Code: 'Y3M'; Name: 'MAZ'),
    (Code: 'Y6D'; Name: 'Zaporozhets'),
    (Code: 'ZAA'; Name: 'Autobianchi'),
    (Code: 'ZAM'; Name: 'Maserati'),
    (Code: 'ZAP'; Name: 'Piaggio/Vespa/Gilera'),
    (Code: 'ZAR'; Name: 'Alfa Romeo'),
    (Code: 'ZA9'; Name: 'Lamborghini'),
    (Code: 'ZBN'; Name: 'Benelli'),
    (Code: 'ZCG'; Name: 'Cagiva SpA / MV Agusta'),
    (Code: 'ZCF'; Name: 'Iveco'),
    (Code: 'ZDC'; Name: 'Honda'),
    (Code: 'ZDM'; Name: 'Ducati'),
    (Code: 'ZDF'; Name: 'Ferrari'),
    (Code: 'ZD0'; Name: 'Yamaha'),
    (Code: 'ZD3'; Name: 'Beta Motor'),
    (Code: 'ZD4'; Name: 'Aprilia'),
    (Code: 'ZFA'; Name: 'Fiat'),
    (Code: 'ZFC'; Name: 'Fiat'),
    (Code: 'ZFF'; Name: 'Ferrari'),
    (Code: 'ZGU'; Name: 'Moto Guzzi'),
    (Code: 'ZHW'; Name: 'Lamborghini'),
    (Code: 'ZJM'; Name: 'Malaguti'),
    (Code: 'ZJN'; Name: 'Innocenti'),
    (Code: 'ZKH'; Name: 'Husqvarna'),
    (Code: 'ZLA'; Name: 'Lancia'),
    (Code: 'Z8M'; Name: 'Marussia'),
    (Code: '137'; Name: 'Hummer'),
    (Code: '1B3'; Name: 'Dodge'),
    (Code: '1C3'; Name: 'Chrysler'),
    (Code: '1C4'; Name: 'Dodge'),
    (Code: '1C6'; Name: 'Chrysler'),
    (Code: '1D3'; Name: 'Dodge'),
    (Code: '1FA'; Name: 'Ford'),
    (Code: '1FB'; Name: 'Ford'),
    (Code: '1FC'; Name: 'Ford'),
    (Code: '1FD'; Name: 'Ford'),
    (Code: '1FM'; Name: 'Ford'),
    (Code: '1FT'; Name: 'Ford'),
    (Code: '1FU'; Name: 'Freightliner'),
    (Code: '1FV'; Name: 'Freightliner'),
    (Code: '1F9'; Name: 'FWD.'),
    (Code: '1G'; Name: 'General Motors'),
    (Code: '1GC'; Name: 'Chevrolet'),
    (Code: '1GT'; Name: 'GMC'),
    (Code: '1G1'; Name: 'Chevrolet'),
    (Code: '1G2'; Name: 'Pontiac'),
    (Code: '1G3'; Name: 'Oldsmobile'),
    (Code: '1G4'; Name: 'Buick'),
    (Code: '1G6'; Name: 'Cadillac'),
    (Code: '1G8'; Name: 'Saturn'),
    (Code: '1GM'; Name: 'Pontiac'),
    (Code: '1GN'; Name: 'Chevrolet'),
    (Code: '1GY'; Name: 'Cadillac'),
    (Code: '1H'; Name: 'Honda'),
    (Code: '1HD'; Name: 'Harley-Davidson'),
    (Code: '1HT'; Name: 'International Truck and Engine Corp'),
    (Code: '1J4'; Name: 'Jeep'),
    (Code: '1J8'; Name: 'Jeep'),
    (Code: '1L'; Name: 'Lincoln'),
    (Code: '1ME'; Name: 'Mercury'),
    (Code: '1M1'; Name: 'Mack'),
    (Code: '1M2'; Name: 'Mack'),
    (Code: '1M3'; Name: 'Mack'),
    (Code: '1M4'; Name: 'Mack'),
    (Code: '1M9'; Name: 'Mynatt'),
    (Code: '1N'; Name: 'Nissan'),
    (Code: '1NX'; Name: 'NUMMI'),
    (Code: '1P3'; Name: 'Plymouth'),
    (Code: '1PY'; Name: 'John Deere'),
    (Code: '1R9'; Name: 'Roadrunner'),
    (Code: '1VW'; Name: 'Volkswagen'),
    (Code: '1XK'; Name: 'Kenworth'),
    (Code: '1XP'; Name: 'Peterbilt'),
    (Code: '1YV'; Name: 'Mazda'),
    (Code: '1ZV'; Name: 'Ford'),
    (Code: '2A4'; Name: 'Chrysler'),
    (Code: '2BP'; Name: 'Bombardier'),
    (Code: '2B3'; Name: 'Dodge'),
    (Code: '2B7'; Name: 'Dodge'),
    (Code: '2C3'; Name: 'Dodge'),
    (Code: '2CN'; Name: 'Chevrolet'),
    (Code: '2D3'; Name: 'Dodge'),
    (Code: '2FA'; Name: 'Ford'),
    (Code: '2FB'; Name: 'Ford'),
    (Code: '2FC'; Name: 'Ford'),
    (Code: '2FM'; Name: 'Ford'),
    (Code: '2FT'; Name: 'Ford'),
    (Code: '2FU'; Name: 'Freightliner'),
    (Code: '2FV'; Name: 'Freightliner'),
    (Code: '2FZ'; Name: 'Sterling'),
    (Code: '2Gx'; Name: 'General Motors'),
    (Code: '2GC'; Name: 'Chevrolet'),
    (Code: '2G1'; Name: 'Chevrolet'),
    (Code: '2G2'; Name: 'Pontiac'),
    (Code: '2G3'; Name: 'Oldsmobile'),
    (Code: '2G4'; Name: 'Buick'),
    (Code: '2HG'; Name: 'Honda'),
    (Code: '2HK'; Name: 'Honda'),
    (Code: '2HJ'; Name: 'Honda'),
    (Code: '2HM'; Name: 'Hyundai'),
    (Code: '2M'; Name: 'Mercury'),
    (Code: '2NV'; Name: 'Nova'),
    (Code: '2P3'; Name: 'Plymouth'),
    (Code: '2T2'; Name: 'Lexus'),
    (Code: '2T'; Name: 'Toyota'),
    (Code: '2TP'; Name: 'Triple E'),
    (Code: '2V4'; Name: 'Volkswagen'),
    (Code: '2V8'; Name: 'Volkswagen'),
    (Code: '2WK'; Name: 'Western Star'),
    (Code: '2WL'; Name: 'Western Star'),
    (Code: '2WM'; Name: 'Western Star'),
    (Code: '363'; Name: 'Spyker'),
    (Code: '3C4'; Name: 'Chrysler'),
    (Code: '3C6'; Name: 'RAM'),
    (Code: '3D3'; Name: 'Dodge'),
    (Code: '3D4'; Name: 'Dodge'),
    (Code: '3FA'; Name: 'Ford'),
    (Code: '3FE'; Name: 'Ford'),
    (Code: '3G'; Name: 'General Motors'),
    (Code: '3H'; Name: 'Honda'),
    (Code: '3JB'; Name: 'BRP'),
    (Code: '3MD'; Name: 'Mazda'),
    (Code: '3MZ'; Name: 'Mazda'),
    (Code: '3N'; Name: 'Nissan'),
    (Code: '3NS'; Name: 'Polaris'),
    (Code: '3NE'; Name: 'Polaris'),
    (Code: '3P3'; Name: 'Plymouth'),
    (Code: '3VW'; Name: 'Volkswagen'),
    (Code: '46J'; Name: 'Federal Motors Inc'),
    (Code: '4EN'; Name: 'Emergency One'),
    (Code: '4F'; Name: 'Mazda'),
    (Code: '4JG'; Name: 'Mercedes-Benz'),
    (Code: '4M'; Name: 'Mercury'),
    (Code: '4P1'; Name: 'Pierce Manufacturing Inc'),
    (Code: '4RK'; Name: 'Nova'),
    (Code: '4S'; Name: 'Subaru-Isuzu'),
    (Code: '4T'; Name: 'Toyota'),
    (Code: '4T9'; Name: 'Lumen Motors'),
    (Code: '4UF'; Name: 'Arctic Cat Inc.'),
    (Code: '4US'; Name: 'BMW'),
    (Code: '4UZ'; Name: 'Frt-Thomas'),
    (Code: '4V1'; Name: 'Volvo'),
    (Code: '4V2'; Name: 'Volvo'),
    (Code: '4V3'; Name: 'Volvo'),
    (Code: '4V4'; Name: 'Volvo'),
    (Code: '4V5'; Name: 'Volvo'),
    (Code: '4V6'; Name: 'Volvo'),
    (Code: '4VL'; Name: 'Volvo'),
    (Code: '4VM'; Name: 'Volvo'),
    (Code: '4VZ'; Name: 'Volvo'),
    (Code: '538'; Name: 'Zero'),
    (Code: '5F'; Name: 'Honda'),
    (Code: '5G'; Name: 'Hummer'),
    (Code: '5J'; Name: 'Honda'),
    (Code: '5L'; Name: 'Lincoln'),
    (Code: '5N1'; Name: 'Infinity'),
    (Code: '5NP'; Name: 'Hyundai'),
    (Code: '5T'; Name: 'Toyota'),
    (Code: '5YJ'; Name: 'Tesla'),
    (Code: '5XY'; Name: 'Kia'),
    (Code: '5UX'; Name: 'BMW'),
    (Code: '56K'; Name: 'Indian'),
    (Code: '6AB'; Name: 'MAN'),
    (Code: '6F4'; Name: 'Nissan'),
    (Code: '6F5'; Name: 'Kenworth'),
    (Code: '6FP'; Name: 'Ford'),
    (Code: '6G1'; Name: 'Holden'),
    (Code: '6G2'; Name: 'Pontiac'),
    (Code: '6H8'; Name: 'Holden'),
    (Code: '6MM'; Name: 'Mitsubishi'),
    (Code: '6T1'; Name: 'Toyota'),
    (Code: '6U9'; Name: 'Privately Imported car in Australia'),
    (Code: '795'; Name: 'Bugatti'),
    (Code: '8AD'; Name: 'Peugeot'),
    (Code: '8AF'; Name: 'Ford'),
    (Code: '8AG'; Name: 'Chevrolet'),
    (Code: '8AJ'; Name: 'Toyota'),
    (Code: '8AK'; Name: 'Suzuki'),
    (Code: '8AP'; Name: 'Fiat'),
    (Code: '8AW'; Name: 'Volkswagen'),
    (Code: '8A1'; Name: 'Renault'),
    (Code: '8GD'; Name: 'Peugeot'),
    (Code: '8GG'; Name: 'Chevrolet'),
    (Code: '8LD'; Name: 'Chevrolet'),
    (Code: '935'; Name: 'Citroën'),
    (Code: '936'; Name: 'Peugeot'),
    (Code: '93H'; Name: 'Honda'),
    (Code: '93R'; Name: 'Toyota'),
    (Code: '93U'; Name: 'Audi'),
    (Code: '93V'; Name: 'Audi'),
    (Code: '93X'; Name: 'Mitsubishi'),
    (Code: '93Y'; Name: 'Renault'),
    (Code: '94D'; Name: 'Nissan'),
    (Code: '9BF'; Name: 'Ford'),
    (Code: '9BG'; Name: 'Chevrolet'),
    (Code: '9BM'; Name: 'Mercedes-Benz'),
    (Code: '9BR'; Name: 'Toyota'),
    (Code: '9BS'; Name: 'Scania'),
    (Code: '9BW'; Name: 'Volkswagen'),
    (Code: '9FB'; Name: 'Renault'),
    (Code: '9GA'; Name: 'Chevrolet')
  );

//------------------------------------------------------------------------------
// VIN ALPHABET CHARACTERS (ONLY CHARACTERS USED IN VIN)
//------------------------------------------------------------------------------
const
  ALPHABET_CHARS: array[0..32] of Char = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'R',
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', '0'
  );

//------------------------------------------------------------------------------
// VIN YEAR ALPHABET CHARACTERS (ONLY CHARACTERS USED FOR THE VIN YEARS)
//------------------------------------------------------------------------------
const
  YEAR_CHARS: array[0..59] of Char = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'R',
    'S', 'T', 'V', 'W', 'X', 'Y', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'R',
    'S', 'T', 'V', 'W', 'X', 'Y', '1', '2', '3', '4', '5', '6', '7', '8', '9'
  );

//------------------------------------------------------------------------------
// VIN COUNTRY MAP
//------------------------------------------------------------------------------
var
  VINCountryMap: TDictionary<string, TVINCountry>;

//------------------------------------------------------------------------------
// VIN MANUFACTURER MAP
//------------------------------------------------------------------------------
var
  VINManufacturerMap: TDictionary<string, TVINManufacturer>;

//------------------------------------------------------------------------------
// VIN YEAR MAP
//------------------------------------------------------------------------------
var
  VINYearMap: TArray<TVINYear>;

implementation

//------------------------------------------------------------------------------
// INITIALIZE COUNTRY MAP
//------------------------------------------------------------------------------
procedure InitializeCountryMap;
var
  Country: TVINCountry;
  StartIndex, EndIndex, I, J, K: Integer;
  Key: string;
begin
  // Create dictionary for the country map
  VINCountryMap := TDictionary<string, TVINCountry>.Create;

  // Loop over the countries
  for Country in VINCountries do
  begin
    // Initialize start index
    StartIndex := -1;
    // Initialize end index
    EndIndex := -1;

    // Find the start and end indexes in the ALPHABET_CHARS array
    for I := Low(ALPHABET_CHARS) to High(ALPHABET_CHARS) do
    begin
      // Find start index
      if ALPHABET_CHARS[I] = Country.RangeStart[1] then StartIndex := I;
      // Find end index
      if ALPHABET_CHARS[I] = Country.RangeEnd[1] then EndIndex := I;
      // Exit if we found the start and end indexes
      if (StartIndex <> -1) and (EndIndex <> -1) then Break;
    end;

    // Skip if range is invalid
    if (StartIndex = -1) or (EndIndex = -1) then Continue;

    // Iterate through the range
    for I := StartIndex to EndIndex do
    for J := Low(ALPHABET_CHARS) to High(ALPHABET_CHARS) do
    begin
      if ALPHABET_CHARS[J] = Country.RangeStart[2] then StartIndex := J;
      if ALPHABET_CHARS[J] = Country.RangeEnd[2] then EndIndex := J;

      for K := StartIndex to EndIndex do
      begin
        Key := Country.RangeStart[1] + ALPHABET_CHARS[K];
        VINCountryMap.AddOrSetValue(Key, Country);
      end;

      // Exit after processing the first character's range
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------
// INITIALIZE MANUFACTURER MAP
//------------------------------------------------------------------------------
procedure InitializeManufacturerMap;
var
  Manufacturer: TVINManufacturer;
  ManufacturerCode, Character: string;
begin
  // Create dictionary for the manufacturer map
  VINManufacturerMap := TDictionary<string, TVINManufacturer>.Create;

  // Loop over the manufacturers
  for Manufacturer in VINManufacturers do
  begin
    // Assign manufacturer code
    ManufacturerCode := Manufacturer.Code;
    // If the manufacturer code is 3 characters long
    // we can use it as-is.
    if Length(ManufacturerCode) = 3 then
    begin
      // Add the manufacturer code to the map
      VINManufacturerMap.AddOrSetValue(ManufacturerCode, Manufacturer);
    end else

    // If the manufacturer code is shorter than 3 characters,
    // than expand the manufacturer codes from the (VIN) alphabet characters.
    if Length(ManufacturerCode) < 3 then
    begin
      // Loop over the (VIN) alphabet characters, and add the code to the map
      for Character in ALPHABET_CHARS do
      begin
        if not VINManufacturerMap.ContainsKey(Manufacturer.Code + Character) then
        VINManufacturerMap.Add(Manufacturer.Code + Character, Manufacturer);
      end;
    end
  end;
end;

//------------------------------------------------------------------------------
// INITIALIZE YEAR MAP
//------------------------------------------------------------------------------
procedure InitializeYearMap;
const
  StartYear: Integer = 1980;
var
  I: Integer;
begin
  // Set length of the map
  SetLength(VINYearMap, Length(YEAR_CHARS));
  // Loop over the year characters and fill the map
  for I := Low(YEAR_CHARS) to High(YEAR_CHARS) do
  begin
    VINYearMap[I].Code := YEAR_CHARS[I];
    VINYearMap[I].Year := StartYear + I;
  end;
end;

//------------------------------------------------------------------------------
// INITIALIZATION
//------------------------------------------------------------------------------
initialization
  // Initialize the country map
  InitializeCountryMap;
  // Initialize the manufacturer map
  InitializeManufacturerMap;
  // Initialize the year map
  InitializeYearMap;

//------------------------------------------------------------------------------
// FINALIZATION
//------------------------------------------------------------------------------
finalization
  // Free the country map
  FreeAndNil(VINCountryMap);
  // Free the manufacturer map
  FreeAndNil(VINManufacturerMap);

end.
