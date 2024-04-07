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
  VINCountries: array[0..131] of TVINCountry = (
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
    (RangeStart: '2A'; RangeEnd: '25'; Name: 'Canada'; Code: 'CA'),
    (RangeStart: '3A'; RangeEnd: '3X'; Name: 'Mexico'; Code: 'MX'),
    (RangeStart: '34'; RangeEnd: '34'; Name: 'Nicaragua'; Code: 'NI'),
    (RangeStart: '35'; RangeEnd: '35'; Name: 'Dominican Republic'; Code: 'DO'),
    (RangeStart: '36'; RangeEnd: '36'; Name: 'Honduras'; Code: 'HN'),
    (RangeStart: '37'; RangeEnd: '37'; Name: 'Panama'; Code: 'PA'),
    (RangeStart: '38'; RangeEnd: '39'; Name: 'Puerto Rico'; Code: 'PR'),
    (RangeStart: '4A'; RangeEnd: '40'; Name: 'United States'; Code: 'US'),
    (RangeStart: '5A'; RangeEnd: '50'; Name: 'United States'; Code: 'US'),
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
  VINManufacturers: array[0..541] of TVINManufacturer = (
    (Code: 'AAV'; Name: 'Volkswagen South Africa'),
    (Code: 'AC5'; Name: 'Hyundai South Africa'),
    (Code: 'ADD'; Name: 'Hyundai South Africa'),
    (Code: 'AFA'; Name: 'Ford South Africa'),
    (Code: 'AHT'; Name: 'Toyota South Africa'),
    (Code: 'JA3'; Name: 'Mitsubishi'),
    (Code: 'JA4'; Name: 'Mitsubishi'),
    (Code: 'JA'; Name: 'Isuzu'),
    (Code: 'JD'; Name: 'Daihatsu'),
    (Code: 'JF'; Name: 'Fuji Heavy Industries (Subaru)'),
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
    (Code: 'JK'; Name: 'Kawasaki (motorcycles)'),
    (Code: 'JL5'; Name: 'Mitsubishi Fuso'),
    (Code: 'JM1'; Name: 'Mazda'),
    (Code: 'JMB'; Name: 'Mitsubishi Motors'),
    (Code: 'JMY'; Name: 'Mitsubishi Motors'),
    (Code: 'JMZ'; Name: 'Mazda'),
    (Code: 'JN'; Name: 'Nissan'),
    (Code: 'JS'; Name: 'Suzuki'),
    (Code: 'JT'; Name: 'Toyota'),
    (Code: 'JY'; Name: 'Yamaha (motorcycles)'),
    (Code: 'KL'; Name: 'Daewoo General Motors South Korea'),
    (Code: 'KM'; Name: 'Hyundai'),
    (Code: 'KMY'; Name: 'Daelim (motorcycles)'),
    (Code: 'KM1'; Name: 'Hyosung (motorcycles)'),
    (Code: 'KN'; Name: 'Kia'),
    (Code: 'KNM'; Name: 'Renault Samsung'),
    (Code: 'KPA'; Name: 'SsangYong'),
    (Code: 'KPT'; Name: 'SsangYong'),
    (Code: 'LAE'; Name: 'Jinan Qingqi Motorcycle'),
    (Code: 'LAL'; Name: 'Sundiro Honda Motorcycle'),
    (Code: 'LAN'; Name: 'Changzhou Yamasaki Motorcycle'),
    (Code: 'LBB'; Name: 'Zhejiang Qianjiang Motorcycle (Keeway/Generic)'),
    (Code: 'LBE'; Name: 'Beijing Hyundai'),
    (Code: 'LBM'; Name: 'Zongshen Piaggio'),
    (Code: 'LBP'; Name: 'Chongqing Jainshe Yamaha (motorcycles)'),
    (Code: 'LB2'; Name: 'Geely Motorcycles'),
    (Code: 'LCE'; Name: 'Hangzhou Chunfeng Motorcycles (CFMOTO)'),
    (Code: 'LDC'; Name: 'Dong Feng Peugeot Citroen (DPCA), China'),
    (Code: 'LDD'; Name: 'Dandong Huanghai Automobile'),
    (Code: 'LDF'; Name: 'Dezhou Fulu Vehicle (motorcycles)'),
    (Code: 'LDN'; Name: 'SouEast Motor'),
    (Code: 'LDY'; Name: 'Zhongtong Coach, China'),
    (Code: 'LET'; Name: 'Jiangling-Isuzu Motors, China'),
    (Code: 'LE4'; Name: 'Beijing Benz, China'),
    (Code: 'LFB'; Name: 'FAW, China (busses)'),
    (Code: 'LFG'; Name: 'Taizhou Chuanl Motorcycle Manufacturing'),
    (Code: 'LFP'; Name: 'FAW, China (passenger vehicles)'),
    (Code: 'LFT'; Name: 'FAW, China (trailers)'),
    (Code: 'LFV'; Name: 'FAW-Volkswagen, China'),
    (Code: 'LFW'; Name: 'FAW JieFang, China'),
    (Code: 'LFY'; Name: 'Changshu Light Motorcycle Factory'),
    (Code: 'LGB'; Name: 'Dong Feng (DFM), China'),
    (Code: 'LGH'; Name: 'Qoros (formerly Dong Feng (DFM)), China'),
    (Code: 'LGX'; Name: 'BYD Auto, China'),
    (Code: 'LHB'; Name: 'Beijing Automotive Industry Holding'),
    (Code: 'LH1'; Name: 'FAW-Haima, China'),
    (Code: 'LJC'; Name: 'JAC, China'),
    (Code: 'LJ1'; Name: 'JAC, China'),
    (Code: 'LKL'; Name: 'Suzhou King Long, China'),
    (Code: 'LL6'; Name: 'Hunan Changfeng Manufacture Joint-Stock'),
    (Code: 'LL8'; Name: 'Linhai (ATV)'),
    (Code: 'LMC'; Name: 'Suzuki Hong Kong (motorcycles)'),
    (Code: 'LPR'; Name: 'Yamaha Hong Kong (motorcycles)'),
    (Code: 'LPS'; Name: 'Polestar (Volvo) (Sweden)'),
    (Code: 'LRW'; Name: 'Tesla, Inc. (Gigafactory Shanghai)'),
    (Code: 'LSG'; Name: 'Shanghai General Motors, China'),
    (Code: 'LSJ'; Name: 'MG Motor UK Limited - SAIC Motor, Shanghai, China'),
    (Code: 'LSV'; Name: 'Shanghai Volkswagen, China'),
    (Code: 'LSY'; Name: 'Brilliance Zhonghua'),
    (Code: 'LTP'; Name: 'National Electric Vehicle Sweden AB (NEVS)'),
    (Code: 'LTV'; Name: 'Toyota Tian Jin'),
    (Code: 'LUC'; Name: 'Guangqi Honda, China'),
    (Code: 'LVS'; Name: 'Ford Chang An'),
    (Code: 'LVV'; Name: 'Chery, China'),
    (Code: 'LVZ'; Name: 'Dong Feng Sokon Motor Company (DFSK)'),
    (Code: 'LV3'; Name: 'National Electric Vehicle Sweden AB (NEVS)'),
    (Code: 'LZM'; Name: 'MAN China'),
    (Code: 'LZE'; Name: 'Isuzu Guangzhou, China'),
    (Code: 'LZG'; Name: 'Shaanxi Automobile Group, China'),
    (Code: 'LZP'; Name: 'Zhongshan Guochi Motorcycle (Baotian)'),
    (Code: 'LZY'; Name: 'Yutong Zhengzhou, China'),
    (Code: 'LZZ'; Name: 'Chongqing Shuangzing Mech & Elec (Howo)'),
    (Code: 'L4B'; Name: 'Xingyue Group (motorcycles)'),
    (Code: 'L5C'; Name: 'KangDi (ATV)'),
    (Code: 'L5K'; Name: 'Zhejiang Yongkang Easy Vehicle'),
    (Code: 'L5N'; Name: 'Zhejiang Taotao, China (ATV & motorcycles)'),
    (Code: 'L5Y'; Name: 'Merato Motorcycle Taizhou Zhongneng'),
    (Code: 'L85'; Name: 'Zhejiang Yongkang Huabao Electric Appliance'),
    (Code: 'L8X'; Name: 'Zhejiang Summit Huawin Motorcycle'),
    (Code: 'MAB'; Name: 'Mahindra & Mahindra'),
    (Code: 'MAC'; Name: 'Mahindra & Mahindra'),
    (Code: 'MAJ'; Name: 'Ford India'),
    (Code: 'MAK'; Name: 'Honda Siel Cars India'),
    (Code: 'MAL'; Name: 'Hyundai India'),
    (Code: 'MAT'; Name: 'Tata Motors'),
    (Code: 'MA1'; Name: 'Mahindra & Mahindra'),
    (Code: 'MA3'; Name: 'Suzuki India (Maruti)'),
    (Code: 'MA6'; Name: 'GM India'),
    (Code: 'MA7'; Name: 'Mitsubishi India (formerly Honda)'),
    (Code: 'MB8'; Name: 'Suzuki India Motorcycles'),
    (Code: 'MBH'; Name: 'Suzuki India (Maruti)'),
    (Code: 'MBJ'; Name: 'Toyota India'),
    (Code: 'MBR'; Name: 'Mercedes-Benz India'),
    (Code: 'MB1'; Name: 'Ashok Leyland'),
    (Code: 'MCA'; Name: 'Fiat India'),
    (Code: 'MCB'; Name: 'GM India'),
    (Code: 'MC2'; Name: 'Volvo Eicher commercial vehicles limited.'),
    (Code: 'MDH'; Name: 'Nissan India'),
    (Code: 'MD2'; Name: 'Bajaj Auto'),
    (Code: 'MD9'; Name: 'Shuttle Cars India'),
    (Code: 'MEC'; Name: 'Daimler India Commercial Vehicles'),
    (Code: 'MEE'; Name: 'Renault India'),
    (Code: 'MEX'; Name: 'Volkswagen India'),
    (Code: 'MHF'; Name: 'Toyota Indonesia'),
    (Code: 'MHR'; Name: 'Honda Indonesia'),
    (Code: 'MLC'; Name: 'Suzuki Thailand'),
    (Code: 'NAA'; Name: 'Iran Khodro (Peugeot Iran)'),
    (Code: 'NAP'; Name: 'Pars Khodro'),
    (Code: 'MLH'; Name: 'Honda Thailand'),
    (Code: 'MMA'; Name: 'Mitsubishi Thailand'),
    (Code: 'MMB'; Name: 'Mitsubishi Thailand'),
    (Code: 'MMC'; Name: 'Mitsubishi Thailand'),
    (Code: 'MMM'; Name: 'Chevrolet Thailand'),
    (Code: 'MMS'; Name: 'Suzuki Thailand'),
    (Code: 'MMT'; Name: 'Mitsubishi Thailand'),
    (Code: 'MMU'; Name: 'Holden Thailand'),
    (Code: 'MM8'; Name: 'Mazda Thailand'),
    (Code: 'MNB'; Name: 'Ford Thailand'),
    (Code: 'MNT'; Name: 'Nissan Thailand'),
    (Code: 'MPA'; Name: 'Isuzu Thailand'),
    (Code: 'MP1'; Name: 'Isuzu Thailand'),
    (Code: 'MRH'; Name: 'Honda Thailand'),
    (Code: 'MR0'; Name: 'Toyota Thailand'),
    (Code: 'MS0'; Name: 'SSS MOTORS Myanmar'),
    (Code: 'MS3'; Name: 'Suzuki Myanmar Motor Co.,Ltd.'),
    (Code: 'NLA'; Name: 'Honda Türkiye'),
    (Code: 'NLE'; Name: 'Mercedes-Benz Türk Truck'),
    (Code: 'NLH'; Name: 'Hyundai Assan'),
    (Code: 'NLN'; Name: 'Karsan'),
    (Code: 'NLR'; Name: 'OTOKAR'),
    (Code: 'NLT'; Name: 'TEMSA'),
    (Code: 'NMB'; Name: 'Mercedes-Benz Türk Buses'),
    (Code: 'NMC'; Name: 'BMC'),
    (Code: 'NM0'; Name: 'Ford Turkey'),
    (Code: 'NM4'; Name: 'Tofaş Türk'),
    (Code: 'NMT'; Name: 'Toyota Türkiye'),
    (Code: 'NNA'; Name: 'Isuzu Turkey'),
    (Code: 'PE1'; Name: 'Ford Philippines'),
    (Code: 'PE3'; Name: 'Mazda Philippines'),
    (Code: 'PL1'; Name: 'Proton, Malaysia'),
    (Code: 'PNA'; Name: 'NAZA, Malaysia (Peugeot)'),
    (Code: 'R2P'; Name: 'Evoke Electric Motorcycles HK'),
    (Code: 'RA1'; Name: 'Steyr Trucks International FZE, UAE'),
    (Code: 'RFB'; Name: 'Kymco, Taiwan'),
    (Code: 'RFG'; Name: 'Sanyang SYM, Taiwan'),
    (Code: 'RFL'; Name: 'Adly, Taiwan'),
    (Code: 'RFT'; Name: 'CPI, Taiwan'),
    (Code: 'RF3'; Name: 'Aeon Motor, Taiwan'),
    (Code: 'SAB'; Name: 'Optare'),
    (Code: 'SAD'; Name: 'Jaguar (F-Pace, I-Pace)'),
    (Code: 'SAL'; Name: 'Land Rover'),
    (Code: 'SAJ'; Name: 'Jaguar'),
    (Code: 'SAR'; Name: 'Rover'),
    (Code: 'SAX'; Name: 'Austin-Rover'),
    (Code: 'SA9'; Name: 'OX Global'),
    (Code: 'SB1'; Name: 'Toyota UK'),
    (Code: 'SBM'; Name: 'McLaren'),
    (Code: 'SCA'; Name: 'Rolls Royce'),
    (Code: 'SCB'; Name: 'Bentley'),
    (Code: 'SCC'; Name: 'Lotus Cars'),
    (Code: 'SCE'; Name: 'DeLorean Motor Cars N. Ireland (UK)'),
    (Code: 'SCF'; Name: 'Aston'),
    (Code: 'SCK'; Name: 'iFor Williams'),
    (Code: 'SDB'; Name: 'Peugeot UK (formerly Talbot)'),
    (Code: 'SED'; Name: 'General Motors Luton Plant'),
    (Code: 'SEY'; Name: 'LDV'),
    (Code: 'SFA'; Name: 'Ford UK'),
    (Code: 'SFD'; Name: 'Alexander Dennis UK'),
    (Code: 'SHH'; Name: 'Honda UK'),
    (Code: 'SHS'; Name: 'Honda UK'),
    (Code: 'SJN'; Name: 'Nissan UK'),
    (Code: 'SKF'; Name: 'Vauxhall'),
    (Code: 'SLP'; Name: 'JCB Research UK'),
    (Code: 'SMT'; Name: 'Triumph Motorcycles'),
    (Code: 'SUF'; Name: 'Fiat Auto Poland'),
    (Code: 'SUL'; Name: 'FSC (Poland)'),
    (Code: 'SUP'; Name: 'FSO-Daewoo (Poland)'),
    (Code: 'SU9'; Name: 'Solaris Bus & Coach (Poland)'),
    (Code: 'SUU'; Name: 'Solaris Bus & Coach (Poland)'),
    (Code: 'SWV'; Name: 'TA-NO (Poland)'),
    (Code: 'TCC'; Name: 'Micro Compact Car AG (smart 1998-1999)'),
    (Code: 'TDM'; Name: 'QUANTYA Swiss Electric Movement (Switzerland)'),
    (Code: 'TK9'; Name: 'SOR buses (Czech Republic)'),
    (Code: 'TMA'; Name: 'Hyundai Motor Manufacturing Czech'),
    (Code: 'TMB'; Name: 'Škoda (Czech Republic)'),
    (Code: 'TMK'; Name: 'Karosa (Czech Republic)'),
    (Code: 'TMP'; Name: 'Škoda trolleybuses (Czech Republic)'),
    (Code: 'TMT'; Name: 'Tatra (Czech Republic)'),
    (Code: 'TM9'; Name: 'Škoda trolleybuses (Czech Republic)'),
    (Code: 'TNE'; Name: 'TAZ'),
    (Code: 'TN9'; Name: 'Karosa (Czech Republic)'),
    (Code: 'TRA'; Name: 'Ikarus Bus'),
    (Code: 'TRU'; Name: 'Audi Hungary'),
    (Code: 'TSB'; Name: 'Ikarus Bus'),
    (Code: 'TSE'; Name: 'Ikarus Egyedi Autobuszgyar, (Hungary)'),
    (Code: 'TSM'; Name: 'Suzuki Hungary'),
    (Code: 'TW1'; Name: 'Toyota Caetano Portugal'),
    (Code: 'TYA'; Name: 'Mitsubishi Trucks Portugal'),
    (Code: 'TYB'; Name: 'Mitsubishi Trucks Portugal'),
    (Code: 'UU1'; Name: 'Renault Dacia, (Romania)'),
    (Code: 'UU2'; Name: 'Oltcit'),
    (Code: 'UU3'; Name: 'ARO'),
    (Code: 'UU4'; Name: 'Roman SA'),
    (Code: 'UU5'; Name: 'Rocar'),
    (Code: 'UU6'; Name: 'Daewoo Romania'),
    (Code: 'UU7'; Name: 'Euro Bus Diamond'),
    (Code: 'UU9'; Name: 'Astra Bus'),
    (Code: 'UV9'; Name: 'ATP Bus'),
    (Code: 'UZT'; Name: 'UTB (Uzina de Tractoare Brașov)'),
    (Code: 'U5Y'; Name: 'Kia Motors Slovakia'),
    (Code: 'U6Y'; Name: 'Kia Motors Slovakia'),
    (Code: 'VAG'; Name: 'Magna Steyr Puch'),
    (Code: 'VAN'; Name: 'MAN Austria'),
    (Code: 'VBK'; Name: 'KTM (Motorcycles)'),
    (Code: 'VF1'; Name: 'Renault'),
    (Code: 'VF2'; Name: 'Renault'),
    (Code: 'VF3'; Name: 'Peugeot'),
    (Code: 'VF4'; Name: 'Talbot'),
    (Code: 'VF6'; Name: 'Renault (Trucks & Buses)'),
    (Code: 'VF7'; Name: 'Citroën'),
    (Code: 'VF8'; Name: 'Matra'),
    (Code: 'VF9'; Name: 'Bugatti'),
    (Code: 'VG5'; Name: 'MBK (motorcycles)'),
    (Code: 'VLU'; Name: 'Scania France'),
    (Code: 'VN1'; Name: 'SOVAB (France)'),
    (Code: 'VNE'; Name: 'Irisbus (France)'),
    (Code: 'VNK'; Name: 'Toyota France'),
    (Code: 'VNV'; Name: 'Renault-Nissan'),
    (Code: 'VSA'; Name: 'Mercedes-Benz Spain'),
    (Code: 'VSE'; Name: 'Suzuki Spain (Santana Motors)'),
    (Code: 'VSK'; Name: 'Nissan Spain'),
    (Code: 'VSS'; Name: 'SEAT'),
    (Code: 'VSX'; Name: 'Opel Spain'),
    (Code: 'VS6'; Name: 'Ford Spain'),
    (Code: 'VS7'; Name: 'Citroën Spain'),
    (Code: 'VS9'; Name: 'Carrocerias Ayats (Spain)'),
    (Code: 'VTH'; Name: 'Derbi (motorcycles)'),
    (Code: 'VTL'; Name: 'Yamaha Spain (motorcycles)'),
    (Code: 'VTT'; Name: 'Suzuki Spain (motorcycles)'),
    (Code: 'VV9'; Name: 'TAURO Spain'),
    (Code: 'VWA'; Name: 'Nissan Spain'),
    (Code: 'VWV'; Name: 'Volkswagen Spain'),
    (Code: 'VX1'; Name: 'Zastava / Yugo Serbia'),
    (Code: 'WAG'; Name: 'Neoplan'),
    (Code: 'WAU'; Name: 'Audi'),
    (Code: 'WA1'; Name: 'Audi SUV'),
    (Code: 'WBA'; Name: 'BMW'),
    (Code: 'WBS'; Name: 'BMW M'),
    (Code: 'WBW'; Name: 'BMW'),
    (Code: 'WBY'; Name: 'BMW'),
    (Code: 'WB1'; Name: 'BMW Motorrad of North America'),
    (Code: 'WDA'; Name: 'Daimler'),
    (Code: 'WDB'; Name: 'Mercedes-Benz'),
    (Code: 'WDC'; Name: 'DaimlerChrysler'),
    (Code: 'WDD'; Name: 'Mercedes-Benz'),
    (Code: 'WDF'; Name: 'Mercedes-Benz (commercial vehicles)'),
    (Code: 'WEB'; Name: 'Evobus GmbH (Mercedes-Bus)'),
    (Code: 'WJM'; Name: 'Iveco Magirus'),
    (Code: 'WF0'; Name: 'Ford Germany'),
    (Code: 'WKE'; Name: 'Fahrzeugwerk Bernard Krone (truck trailers)'),
    (Code: 'WKK'; Name: 'Kässbohrer/Setra'),
    (Code: 'WMA'; Name: 'MAN Germany'),
    (Code: 'WME'; Name: 'smart'),
    (Code: 'WMW'; Name: 'MINI'),
    (Code: 'WMX'; Name: 'Mercedes-AMG'),
    (Code: 'WMZ'; Name: 'MINI'),
    (Code: 'WP0'; Name: 'Porsche'),
    (Code: 'WP1'; Name: 'Porsche SUV'),
    (Code: 'WSM'; Name: 'Schmitz-Cargobull (truck trailers)'),
    (Code: 'W09'; Name: 'RUF'),
    (Code: 'W0L'; Name: 'Opel'),
    (Code: 'W0V'; Name: 'Opel (since 2017)'),
    (Code: 'WUA'; Name: 'Audi Sport GmbH (formerly quattro GmbH)'),
    (Code: 'WVG'; Name: 'Volkswagen MPV/SUV'),
    (Code: 'WVW'; Name: 'Volkswagen'),
    (Code: 'WV1'; Name: 'Volkswagen Commercial Vehicles'),
    (Code: 'WV2'; Name: 'Volkswagen Bus/Van'),
    (Code: 'WV3'; Name: 'Volkswagen Trucks'),
    (Code: 'XLB'; Name: 'Volvo (NedCar)'),
    (Code: 'XLE'; Name: 'Scania Netherlands'),
    (Code: 'XLR'; Name: 'DAF (trucks)'),
    (Code: 'XL4'; Name: 'Lightyear'),
    (Code: 'XL9'; Name: 'Spyker'),
    (Code: 'XMC'; Name: 'Mitsubishi (NedCar)'),
    (Code: 'XMG'; Name: 'VDL Bus & Coach'),
    (Code: 'XTA'; Name: 'Lada/AvtoVAZ (Russia)'),
    (Code: 'XTC'; Name: 'KAMAZ (Russia)'),
    (Code: 'XTH'; Name: 'GAZ (Russia)'),
    (Code: 'XTT'; Name: 'UAZ/Sollers (Russia)'),
    (Code: 'XTU'; Name: 'Trolza (Russia)'),
    (Code: 'XTY'; Name: 'LiAZ (Russia)'),
    (Code: 'XUF'; Name: 'General Motors Russia'),
    (Code: 'XUU'; Name: 'AvtoTor (Russia, General Motors SKD)'),
    (Code: 'XW8'; Name: 'Volkswagen Group Russia'),
    (Code: 'XWB'; Name: 'UZ-Daewoo (Uzbekistan)'),
    (Code: 'XWE'; Name: 'AvtoTor (Russia, Hyundai-Kia SKD)'),
    (Code: 'X1M'; Name: 'PAZ (Russia)'),
    (Code: 'X4X'; Name: 'AvtoTor (Russia, BMW SKD)'),
    (Code: 'X7L'; Name: 'Renault AvtoFramos (Russia)'),
    (Code: 'X7M'; Name: 'Hyundai TagAZ (Russia)'),
    (Code: 'YBW'; Name: 'Volkswagen Belgium'),
    (Code: 'YB1'; Name: 'Volvo Trucks Belgium'),
    (Code: 'YCM'; Name: 'Mazda Belgium'),
    (Code: 'YE2'; Name: 'Van Hool (buses)'),
    (Code: 'YH2'; Name: 'BRP Finland (Lynx snowmobiles)'),
    (Code: 'YK1'; Name: 'Saab-Valmet Finland'),
    (Code: 'YSC'; Name: 'Cadillac (Saab)'),
    (Code: 'YS2'; Name: 'Scania AB'),
    (Code: 'YS3'; Name: 'Saab'),
    (Code: 'YS4'; Name: 'Scania Bus'),
    (Code: 'YTN'; Name: 'Saab NEVS'),
    (Code: 'YT9'; Name: 'Koenigsegg'),
    (Code: '007'; Name: 'Koenigsegg'),
    (Code: 'YT9'; Name: 'Carvia'),
    (Code: '034'; Name: 'Carvia'),
    (Code: 'YU7'; Name: 'Husaberg (motorcycles)'),
    (Code: 'YVV'; Name: 'Polestar (Volvo) (Sweden)'),
    (Code: 'YV1'; Name: 'Volvo Cars'),
    (Code: 'YV4'; Name: 'Volvo Cars'),
    (Code: 'YV2'; Name: 'Volvo Trucks'),
    (Code: 'YV3'; Name: 'Volvo Buses'),
    (Code: 'Y3M'; Name: 'MAZ (Belarus)'),
    (Code: 'Y6D'; Name: 'Zaporozhets/AvtoZAZ (Ukraine)'),
    (Code: 'ZAA'; Name: 'Autobianchi'),
    (Code: 'ZAM'; Name: 'Maserati'),
    (Code: 'ZAP'; Name: 'Piaggio/Vespa/Gilera'),
    (Code: 'ZAR'; Name: 'Alfa Romeo'),
    (Code: 'ZBN'; Name: 'Benelli'),
    (Code: 'ZCG'; Name: 'Cagiva SpA / MV Agusta'),
    (Code: 'ZCF'; Name: 'Iveco'),
    (Code: 'ZDC'; Name: 'Honda Italia Industriale SpA'),
    (Code: 'ZDM'; Name: 'Ducati Motor Holdings SpA'),
    (Code: 'ZDF'; Name: 'Ferrari Dino'),
    (Code: 'ZD0'; Name: 'Yamaha Italy'),
    (Code: 'ZD3'; Name: 'Beta Motor'),
    (Code: 'ZD4'; Name: 'Aprilia'),
    (Code: 'ZFA'; Name: 'Fiat'),
    (Code: 'ZFC'; Name: 'Fiat V.I.'),
    (Code: 'ZFF'; Name: 'Ferrari'),
    (Code: 'ZGU'; Name: 'Moto Guzzi'),
    (Code: 'ZHW'; Name: 'Lamborghini'),
    (Code: 'ZJM'; Name: 'Malaguti'),
    (Code: 'ZJN'; Name: 'Innocenti'),
    (Code: 'ZKH'; Name: 'Husqvarna Motorcycles Italy'),
    (Code: 'ZLA'; Name: 'Lancia'),
    (Code: 'Z8M'; Name: 'Marussia (Russia)'),
    (Code: '1B3'; Name: 'Dodge'),
    (Code: '1C3'; Name: 'Chrysler'),
    (Code: '1C4'; Name: 'Chrysler'),
    (Code: '1C6'; Name: 'Chrysler'),
    (Code: '1D3'; Name: 'Dodge'),
    (Code: '1FA'; Name: 'Ford Motor Company'),
    (Code: '1FB'; Name: 'Ford Motor Company'),
    (Code: '1FC'; Name: 'Ford Motor Company'),
    (Code: '1FD'; Name: 'Ford Motor Company'),
    (Code: '1FM'; Name: 'Ford Motor Company'),
    (Code: '1FT'; Name: 'Ford Motor Company'),
    (Code: '1FU'; Name: 'Freightliner'),
    (Code: '1FV'; Name: 'Freightliner'),
    (Code: '1F9'; Name: 'FWD Corp.'),
    (Code: '1G'; Name: 'General Motors USA'),
    (Code: '1GC'; Name: 'Chevrolet Truck USA'),
    (Code: '1GT'; Name: 'GMC Truck USA'),
    (Code: '1G1'; Name: 'Chevrolet USA'),
    (Code: '1G2'; Name: 'Pontiac USA'),
    (Code: '1G3'; Name: 'Oldsmobile USA'),
    (Code: '1G4'; Name: 'Buick USA'),
    (Code: '1G6'; Name: 'Cadillac USA'),
    (Code: '1G8'; Name: 'Saturn USA'),
    (Code: '1GM'; Name: 'Pontiac USA'),
    (Code: '1GN'; Name: 'Chevrolet SUV USA'),
    (Code: '1GY'; Name: 'Cadillac USA'),
    (Code: '1H'; Name: 'Honda USA'),
    (Code: '1HD'; Name: 'Harley-Davidson'),
    (Code: '1HT'; Name: 'International Truck and Engine Corp. USA'),
    (Code: '1J4'; Name: 'Jeep'),
    (Code: '1J8'; Name: 'Jeep'),
    (Code: '1L'; Name: 'Lincoln USA'),
    (Code: '1ME'; Name: 'Mercury USA'),
    (Code: '1M1'; Name: 'Mack Truck USA'),
    (Code: '1M2'; Name: 'Mack Truck USA'),
    (Code: '1M3'; Name: 'Mack Truck USA'),
    (Code: '1M4'; Name: 'Mack Truck USA'),
    (Code: '1M9'; Name: 'Mynatt Truck & Equipment'),
    (Code: '1N'; Name: 'Nissan USA'),
    (Code: '1NX'; Name: 'NUMMI USA'),
    (Code: '1P3'; Name: 'Plymouth USA'),
    (Code: '1PY'; Name: 'John Deere USA'),
    (Code: '1R9'; Name: 'Roadrunner Hay Squeeze USA'),
    (Code: '1VW'; Name: 'Volkswagen USA'),
    (Code: '1XK'; Name: 'Kenworth USA'),
    (Code: '1XP'; Name: 'Peterbilt USA'),
    (Code: '1YV'; Name: 'Mazda USA (AutoAlliance International)'),
    (Code: '1ZV'; Name: 'Ford (AutoAlliance International)'),
    (Code: '2A4'; Name: 'Chrysler Canada'),
    (Code: '2BP'; Name: 'Bombardier Recreational Products'),
    (Code: '2B3'; Name: 'Dodge Canada'),
    (Code: '2B7'; Name: 'Dodge Canada'),
    (Code: '2C3'; Name: 'Chrysler Canada'),
    (Code: '2CN'; Name: 'CAMI'),
    (Code: '2D3'; Name: 'Dodge Canada'),
    (Code: '2FA'; Name: 'Ford Motor Company Canada'),
    (Code: '2FB'; Name: 'Ford Motor Company Canada'),
    (Code: '2FC'; Name: 'Ford Motor Company Canada'),
    (Code: '2FM'; Name: 'Ford Motor Company Canada'),
    (Code: '2FT'; Name: 'Ford Motor Company Canada'),
    (Code: '2FU'; Name: 'Freightliner'),
    (Code: '2FV'; Name: 'Freightliner'),
    (Code: '2FZ'; Name: 'Sterling'),
    (Code: '2Gx'; Name: 'General Motors Canada'),
    (Code: '2G1'; Name: 'Chevrolet Canada'),
    (Code: '2G2'; Name: 'Pontiac Canada'),
    (Code: '2G3'; Name: 'Oldsmobile Canada'),
    (Code: '2G4'; Name: 'Buick Canada'),
    (Code: '2G9'; Name: 'mfr. of less than 1000/ yr. Canada'),
    (Code: '2HG'; Name: 'Honda Canada'),
    (Code: '2HK'; Name: 'Honda Canada'),
    (Code: '2HJ'; Name: 'Honda Canada'),
    (Code: '2HM'; Name: 'Hyundai Canada'),
    (Code: '2M'; Name: 'Mercury'),
    (Code: '2NV'; Name: 'Nova Bus Canada'),
    (Code: '2P3'; Name: 'Plymouth Canada'),
    (Code: '2T'; Name: 'Toyota Canada'),
    (Code: '2TP'; Name: 'Triple E Canada LTD'),
    (Code: '2V4'; Name: 'Volkswagen Canada'),
    (Code: '2V8'; Name: 'Volkswagen Canada'),
    (Code: '2WK'; Name: 'Western Star'),
    (Code: '2WL'; Name: 'Western Star'),
    (Code: '2WM'; Name: 'Western Star'),
    (Code: '363'; Name: 'Spyker'),
    (Code: '3C4'; Name: 'Chrysler Mexico'),
    (Code: '3C6'; Name: 'RAM Mexico'),
    (Code: '3D3'; Name: 'Dodge Mexico'),
    (Code: '3D4'; Name: 'Dodge Mexico'),
    (Code: '3FA'; Name: 'Ford Motor Company Mexico'),
    (Code: '3FE'; Name: 'Ford Motor Company Mexico'),
    (Code: '3G'; Name: 'General Motors Mexico'),
    (Code: '3H'; Name: 'Honda Mexico'),
    (Code: '3JB'; Name: 'BRP Mexico (all-terrain vehicles)'),
    (Code: '3MD'; Name: 'Mazda Mexico'),
    (Code: '3MZ'; Name: 'Mazda Mexico'),
    (Code: '3N'; Name: 'Nissan Mexico'),
    (Code: '3NS'; Name: 'Polaris Industries USA'),
    (Code: '3NE'; Name: 'Polaris Industries USA'),
    (Code: '3P3'; Name: 'Plymouth Mexico'),
    (Code: '3VW'; Name: 'Volkswagen Mexico'),
    (Code: '46J'; Name: 'Federal Motors Inc. USA'),
    (Code: '4EN'; Name: 'Emergency One USA'),
    (Code: '4F'; Name: 'Mazda USA'),
    (Code: '4JG'; Name: 'Mercedes-Benz USA'),
    (Code: '4M'; Name: 'Mercury'),
    (Code: '4P1'; Name: 'Pierce Manufacturing Inc. USA'),
    (Code: '4RK'; Name: 'Nova Bus USA'),
    (Code: '4S'; Name: 'Subaru-Isuzu Automotive'),
    (Code: '4T'; Name: 'Toyota'),
    (Code: '4T9'; Name: 'Lumen Motors'),
    (Code: '4UF'; Name: 'Arctic Cat Inc.'),
    (Code: '4US'; Name: 'BMW USA'),
    (Code: '4UZ'; Name: 'Frt-Thomas Bus'),
    (Code: '4V1'; Name: 'Volvo'),
    (Code: '4V2'; Name: 'Volvo'),
    (Code: '4V3'; Name: 'Volvo'),
    (Code: '4V4'; Name: 'Volvo'),
    (Code: '4V5'; Name: 'Volvo'),
    (Code: '4V6'; Name: 'Volvo'),
    (Code: '4VL'; Name: 'Volvo'),
    (Code: '4VM'; Name: 'Volvo'),
    (Code: '4VZ'; Name: 'Volvo'),
    (Code: '538'; Name: 'Zero Motorcycles (USA)'),
    (Code: '5F'; Name: 'Honda USA-Alabama'),
    (Code: '5J'; Name: 'Honda USA-Ohio'),
    (Code: '5L'; Name: 'Lincoln'),
    (Code: '5N1'; Name: 'Nissan USA'),
    (Code: '5NP'; Name: 'Hyundai USA'),
    (Code: '5T'; Name: 'Toyota USA - trucks'),
    (Code: '5YJ'; Name: 'Tesla, Inc.'),
    (Code: '56K'; Name: 'Indian Motorcycle USA'),
    (Code: '6AB'; Name: 'MAN Australia'),
    (Code: '6F4'; Name: 'Nissan Motor Company Australia'),
    (Code: '6F5'; Name: 'Kenworth Australia'),
    (Code: '6FP'; Name: 'Ford Motor Company Australia'),
    (Code: '6G1'; Name: 'General Motors-Holden (post Nov 2002)'),
    (Code: '6G2'; Name: 'Pontiac Australia (GTO & G8)'),
    (Code: '6H8'; Name: 'General Motors-Holden (pre Nov 2002)'),
    (Code: '6MM'; Name: 'Mitsubishi Motors Australia'),
    (Code: '6T1'; Name: 'Toyota Motor Corporation Australia'),
    (Code: '6U9'; Name: 'Privately Imported car in Australia'),
    (Code: '795'; Name: 'Bugatti'),
    (Code: '8AD'; Name: 'Peugeot Argentina'),
    (Code: '8AF'; Name: 'Ford Motor Company Argentina'),
    (Code: '8AG'; Name: 'Chevrolet Argentina'),
    (Code: '8AJ'; Name: 'Toyota Argentina'),
    (Code: '8AK'; Name: 'Suzuki Argentina'),
    (Code: '8AP'; Name: 'Fiat Argentina'),
    (Code: '8AW'; Name: 'Volkswagen Argentina'),
    (Code: '8A1'; Name: 'Renault Argentina'),
    (Code: '8GD'; Name: 'Peugeot Chile'),
    (Code: '8GG'; Name: 'Chevrolet Chile'),
    (Code: '8LD'; Name: 'Chevrolet Ecuador'),
    (Code: '935'; Name: 'Citroën Brazil'),
    (Code: '936'; Name: 'Peugeot Brazil'),
    (Code: '93H'; Name: 'Honda Brazil'),
    (Code: '93R'; Name: 'Toyota Brazil'),
    (Code: '93U'; Name: 'Audi Brazil'),
    (Code: '93V'; Name: 'Audi Brazil'),
    (Code: '93X'; Name: 'Mitsubishi Motors Brazil'),
    (Code: '93Y'; Name: 'Renault Brazil'),
    (Code: '94D'; Name: 'Nissan Brazil'),
    (Code: '9BF'; Name: 'Ford Motor Company Brazil'),
    (Code: '9BG'; Name: 'Chevrolet Brazil'),
    (Code: '9BM'; Name: 'Mercedes-Benz Brazil'),
    (Code: '9BR'; Name: 'Toyota Brazil'),
    (Code: '9BS'; Name: 'Scania Brazil'),
    (Code: '9BW'; Name: 'Volkswagen Brazil'),
    (Code: '9FB'; Name: 'Renault Colombia'),
    (Code: '9GA'; Name: 'Chevrolet Colombia')
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
  YEAR_CHARS: array[0..30] of Char = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'L', 'M', 'N', 'P', 'R',
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', '1', '2', '3', '4', '5', '6', '7', '8', '9'
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
  VINYearMap: TDictionary<Char, Integer>;

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
      for Character in ALPHABET_CHARS do VINManufacturerMap.AddOrSetValue(Manufacturer.Code + Character, Manufacturer);
    end
  end;
end;

//------------------------------------------------------------------------------
// INITIALIZE YEAR MAP
//------------------------------------------------------------------------------
procedure InitializeYearMap;
var
  I: Integer;
begin
  // Create dictionary for the year map
  VINYearMap := TDictionary<Char, Integer>.Create;
  // Loop over the year characters and fill the map
  for I := Low(YEAR_CHARS) to High(YEAR_CHARS) do VINYearMap.AddOrSetValue(YEAR_CHARS[I], I);
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
  // Free the year map
  FreeAndNil(VINYearMap);

end.
