set more off

clear
quietly infix                  ///
  int     year        1-4      ///
  byte    datanum     5-6      ///
  double  serial      7-14     ///
  float   hhwt        15-24    ///
  byte    gq          25-25    ///
  int     pernum      26-29    ///
  float   perwt       30-39    ///
  byte    sex         40-40    ///
  int     age         41-43    ///
  byte    race        44-44    ///
  int     raced       45-47    ///
  int     bpl         48-50    ///
  long    bpld        51-55    ///
  byte    citizen     56-56    ///
  byte    educ        57-58    ///
  int     educd       59-61    ///
  byte    schltype    62-62    ///
  byte    degfield    63-64    ///
  int     degfieldd   65-68    ///
  byte    degfield2   69-70    ///
  int     degfield2d  71-74    ///
  byte    empstat     75-75    ///
  byte    empstatd    76-77    ///
  int     occ         78-81    ///
  int     ind         82-85    ///
  byte    classwkr    86-86    ///
  byte    classwkrd   87-88    ///
  str     occsoc      89-94    ///
  str     indnaics    95-102   ///
  float   presgl      103-105  ///
  float   prent       106-108  ///
  byte    pwstate2    109-110  ///
  byte    qdegfield   111-111  ///
  byte    qschool     112-112  ///
  byte    qclasswk    113-113  ///
  byte    qempstat    114-114  ///
  byte    qind        115-115  ///
  byte    qocc        116-116  ///
  using `"usa_00003.dat"'

replace hhwt       = hhwt       / 100
replace perwt      = perwt      / 100
replace presgl     = presgl     / 10
replace prent      = prent      / 10

format serial     %8.0f
format hhwt       %10.2f
format perwt      %10.2f
format presgl     %3.1f
format prent      %3.1f

label var year       `"Census year"'
label var datanum    `"Data set number"'
label var serial     `"Household serial number"'
label var hhwt       `"Household weight"'
label var gq         `"Group quarters status"'
label var pernum     `"Person number in sample unit"'
label var perwt      `"Person weight"'
label var sex        `"Sex"'
label var age        `"Age"'
label var race       `"Race [general version]"'
label var raced      `"Race [detailed version]"'
label var bpl        `"Birthplace [general version]"'
label var bpld       `"Birthplace [detailed version]"'
label var citizen    `"Citizenship status"'
label var educ       `"Educational attainment [general version]"'
label var educd      `"Educational attainment [detailed version]"'
label var schltype   `"Public or private school"'
label var degfield   `"Field of degree [general version]"'
label var degfieldd  `"Field of degree [detailed version]"'
label var degfield2  `"Field of degree (2) [general version]"'
label var degfield2d `"Field of degree (2) [detailed version]"'
label var empstat    `"Employment status [general version]"'
label var empstatd   `"Employment status [detailed version]"'
label var occ        `"Occupation"'
label var ind        `"Industry"'
label var classwkr   `"Class of worker [general version]"'
label var classwkrd  `"Class of worker [detailed version]"'
label var occsoc     `"Occupation, SOC classification"'
label var indnaics   `"Industry, NAICS classification"'
label var presgl     `"Occupational prestige score, Siegel"'
label var prent      `"Occupational prestige score, Nakao and Treas"'
label var pwstate2   `"Place of work: state"'
label var qdegfield  `"Data quality flag for DEGFIELD and DEGFIELD2"'
label var qschool    `"Flag for School, Schltype"'
label var qclasswk   `"Flag for Classwkr"'
label var qempstat   `"Flag for Empstat, Labforce"'
label var qind       `"Flag for Ind, Ind1950"'
label var qocc       `"Flag for Occ, Occ1950, SEI, Occscore, Occsoc, Labforce"'

label define year_lbl 1850 `"1850"'
label define year_lbl 1860 `"1860"', add
label define year_lbl 1870 `"1870"', add
label define year_lbl 1880 `"1880"', add
label define year_lbl 1900 `"1900"', add
label define year_lbl 1910 `"1910"', add
label define year_lbl 1920 `"1920"', add
label define year_lbl 1930 `"1930"', add
label define year_lbl 1940 `"1940"', add
label define year_lbl 1950 `"1950"', add
label define year_lbl 1960 `"1960"', add
label define year_lbl 1970 `"1970"', add
label define year_lbl 1980 `"1980"', add
label define year_lbl 1990 `"1990"', add
label define year_lbl 2000 `"2000"', add
label define year_lbl 2001 `"2001"', add
label define year_lbl 2002 `"2002"', add
label define year_lbl 2003 `"2003"', add
label define year_lbl 2004 `"2004"', add
label define year_lbl 2005 `"2005"', add
label define year_lbl 2006 `"2006"', add
label define year_lbl 2007 `"2007"', add
label define year_lbl 2008 `"2008"', add
label define year_lbl 2009 `"2009"', add
label define year_lbl 2010 `"2010"', add
label define year_lbl 2011 `"2011"', add
label define year_lbl 2012 `"2012"', add
label define year_lbl 2013 `"2013"', add
label define year_lbl 2014 `"2014"', add
label define year_lbl 2015 `"2015"', add
label values year year_lbl

label define gq_lbl 0 `"Vacant unit"'
label define gq_lbl 1 `"Households under 1970 definition"', add
label define gq_lbl 2 `"Additional households under 1990 definition"', add
label define gq_lbl 3 `"Group quarters--Institutions"', add
label define gq_lbl 4 `"Other group quarters"', add
label define gq_lbl 5 `"Additional households under 2000 definition"', add
label define gq_lbl 6 `"Fragment"', add
label values gq gq_lbl

label define sex_lbl 1 `"Male"'
label define sex_lbl 2 `"Female"', add
label values sex sex_lbl

label define age_lbl 000 `"Less than 1 year old"'
label define age_lbl 001 `"1"', add
label define age_lbl 002 `"2"', add
label define age_lbl 003 `"3"', add
label define age_lbl 004 `"4"', add
label define age_lbl 005 `"5"', add
label define age_lbl 006 `"6"', add
label define age_lbl 007 `"7"', add
label define age_lbl 008 `"8"', add
label define age_lbl 009 `"9"', add
label define age_lbl 010 `"10"', add
label define age_lbl 011 `"11"', add
label define age_lbl 012 `"12"', add
label define age_lbl 013 `"13"', add
label define age_lbl 014 `"14"', add
label define age_lbl 015 `"15"', add
label define age_lbl 016 `"16"', add
label define age_lbl 017 `"17"', add
label define age_lbl 018 `"18"', add
label define age_lbl 019 `"19"', add
label define age_lbl 020 `"20"', add
label define age_lbl 021 `"21"', add
label define age_lbl 022 `"22"', add
label define age_lbl 023 `"23"', add
label define age_lbl 024 `"24"', add
label define age_lbl 025 `"25"', add
label define age_lbl 026 `"26"', add
label define age_lbl 027 `"27"', add
label define age_lbl 028 `"28"', add
label define age_lbl 029 `"29"', add
label define age_lbl 030 `"30"', add
label define age_lbl 031 `"31"', add
label define age_lbl 032 `"32"', add
label define age_lbl 033 `"33"', add
label define age_lbl 034 `"34"', add
label define age_lbl 035 `"35"', add
label define age_lbl 036 `"36"', add
label define age_lbl 037 `"37"', add
label define age_lbl 038 `"38"', add
label define age_lbl 039 `"39"', add
label define age_lbl 040 `"40"', add
label define age_lbl 041 `"41"', add
label define age_lbl 042 `"42"', add
label define age_lbl 043 `"43"', add
label define age_lbl 044 `"44"', add
label define age_lbl 045 `"45"', add
label define age_lbl 046 `"46"', add
label define age_lbl 047 `"47"', add
label define age_lbl 048 `"48"', add
label define age_lbl 049 `"49"', add
label define age_lbl 050 `"50"', add
label define age_lbl 051 `"51"', add
label define age_lbl 052 `"52"', add
label define age_lbl 053 `"53"', add
label define age_lbl 054 `"54"', add
label define age_lbl 055 `"55"', add
label define age_lbl 056 `"56"', add
label define age_lbl 057 `"57"', add
label define age_lbl 058 `"58"', add
label define age_lbl 059 `"59"', add
label define age_lbl 060 `"60"', add
label define age_lbl 061 `"61"', add
label define age_lbl 062 `"62"', add
label define age_lbl 063 `"63"', add
label define age_lbl 064 `"64"', add
label define age_lbl 065 `"65"', add
label define age_lbl 066 `"66"', add
label define age_lbl 067 `"67"', add
label define age_lbl 068 `"68"', add
label define age_lbl 069 `"69"', add
label define age_lbl 070 `"70"', add
label define age_lbl 071 `"71"', add
label define age_lbl 072 `"72"', add
label define age_lbl 073 `"73"', add
label define age_lbl 074 `"74"', add
label define age_lbl 075 `"75"', add
label define age_lbl 076 `"76"', add
label define age_lbl 077 `"77"', add
label define age_lbl 078 `"78"', add
label define age_lbl 079 `"79"', add
label define age_lbl 080 `"80"', add
label define age_lbl 081 `"81"', add
label define age_lbl 082 `"82"', add
label define age_lbl 083 `"83"', add
label define age_lbl 084 `"84"', add
label define age_lbl 085 `"85"', add
label define age_lbl 086 `"86"', add
label define age_lbl 087 `"87"', add
label define age_lbl 088 `"88"', add
label define age_lbl 089 `"89"', add
label define age_lbl 090 `"90 (90+ in 1980 and 1990)"', add
label define age_lbl 091 `"91"', add
label define age_lbl 092 `"92"', add
label define age_lbl 093 `"93"', add
label define age_lbl 094 `"94"', add
label define age_lbl 095 `"95"', add
label define age_lbl 096 `"96"', add
label define age_lbl 097 `"97"', add
label define age_lbl 098 `"98"', add
label define age_lbl 099 `"99"', add
label define age_lbl 100 `"100 (100+ in 1960-1970)"', add
label define age_lbl 101 `"101"', add
label define age_lbl 102 `"102"', add
label define age_lbl 103 `"103"', add
label define age_lbl 104 `"104"', add
label define age_lbl 105 `"105"', add
label define age_lbl 106 `"106"', add
label define age_lbl 107 `"107"', add
label define age_lbl 108 `"108"', add
label define age_lbl 109 `"109"', add
label define age_lbl 110 `"110"', add
label define age_lbl 111 `"111"', add
label define age_lbl 112 `"112 (112+ in the 1980 internal data)"', add
label define age_lbl 113 `"113"', add
label define age_lbl 114 `"114"', add
label define age_lbl 115 `"115 (115+ in the 1990 internal data)"', add
label define age_lbl 116 `"116"', add
label define age_lbl 117 `"117"', add
label define age_lbl 118 `"118"', add
label define age_lbl 119 `"119"', add
label define age_lbl 120 `"120"', add
label define age_lbl 121 `"121"', add
label define age_lbl 122 `"122"', add
label define age_lbl 123 `"123"', add
label define age_lbl 124 `"124"', add
label define age_lbl 125 `"125"', add
label define age_lbl 126 `"126"', add
label define age_lbl 129 `"129"', add
label define age_lbl 130 `"130"', add
label define age_lbl 135 `"135"', add
label values age age_lbl

label define race_lbl 1 `"White"'
label define race_lbl 2 `"Black/Negro"', add
label define race_lbl 3 `"American Indian or Alaska Native"', add
label define race_lbl 4 `"Chinese"', add
label define race_lbl 5 `"Japanese"', add
label define race_lbl 6 `"Other Asian or Pacific Islander"', add
label define race_lbl 7 `"Other race, nec"', add
label define race_lbl 8 `"Two major races"', add
label define race_lbl 9 `"Three or more major races"', add
label values race race_lbl

label define raced_lbl 100 `"White"'
label define raced_lbl 110 `"Spanish write_in"', add
label define raced_lbl 120 `"Blank (white) (1850)"', add
label define raced_lbl 130 `"Portuguese"', add
label define raced_lbl 140 `"Mexican (1930)"', add
label define raced_lbl 150 `"Puerto Rican (1910 Hawaii)"', add
label define raced_lbl 200 `"Black/Negro"', add
label define raced_lbl 210 `"Mulatto"', add
label define raced_lbl 300 `"American Indian/Alaska Native"', add
label define raced_lbl 302 `"Apache"', add
label define raced_lbl 303 `"Blackfoot"', add
label define raced_lbl 304 `"Cherokee"', add
label define raced_lbl 305 `"Cheyenne"', add
label define raced_lbl 306 `"Chickasaw"', add
label define raced_lbl 307 `"Chippewa"', add
label define raced_lbl 308 `"Choctaw"', add
label define raced_lbl 309 `"Comanche"', add
label define raced_lbl 310 `"Creek"', add
label define raced_lbl 311 `"Crow"', add
label define raced_lbl 312 `"Iroquois"', add
label define raced_lbl 313 `"Kiowa"', add
label define raced_lbl 314 `"Lumbee"', add
label define raced_lbl 315 `"Navajo"', add
label define raced_lbl 316 `"Osage"', add
label define raced_lbl 317 `"Paiute"', add
label define raced_lbl 318 `"Pima"', add
label define raced_lbl 319 `"Potawatomi"', add
label define raced_lbl 320 `"Pueblo"', add
label define raced_lbl 321 `"Seminole"', add
label define raced_lbl 322 `"Shoshone"', add
label define raced_lbl 323 `"Sioux"', add
label define raced_lbl 324 `"Tlingit (Tlingit_Haida, 2000/ACS)"', add
label define raced_lbl 325 `"Tohono O Odham"', add
label define raced_lbl 326 `"All other tribes (1990)"', add
label define raced_lbl 328 `"Hopi"', add
label define raced_lbl 329 `"Central American Indian"', add
label define raced_lbl 330 `"Spanish American Indian"', add
label define raced_lbl 350 `"Delaware"', add
label define raced_lbl 351 `"Latin American Indian"', add
label define raced_lbl 352 `"Puget Sound Salish"', add
label define raced_lbl 353 `"Yakama"', add
label define raced_lbl 354 `"Yaqui"', add
label define raced_lbl 355 `"Colville"', add
label define raced_lbl 356 `"Houma"', add
label define raced_lbl 357 `"Menominee"', add
label define raced_lbl 358 `"Yuman"', add
label define raced_lbl 359 `"South American Indian"', add
label define raced_lbl 360 `"Mexican American Indian"', add
label define raced_lbl 361 `"Other Amer. Indian tribe (2000,ACS)"', add
label define raced_lbl 362 `"2+ Amer. Indian tribes (2000,ACS)"', add
label define raced_lbl 370 `"Alaskan Athabaskan"', add
label define raced_lbl 371 `"Aleut"', add
label define raced_lbl 372 `"Eskimo"', add
label define raced_lbl 373 `"Alaskan mixed"', add
label define raced_lbl 374 `"Inupiat"', add
label define raced_lbl 375 `"Yup'ik"', add
label define raced_lbl 379 `"Other Alaska Native tribe(s) (2000,ACS)"', add
label define raced_lbl 398 `"Both Am. Ind. and Alaska Native (2000,ACS)"', add
label define raced_lbl 399 `"Tribe not specified"', add
label define raced_lbl 400 `"Chinese"', add
label define raced_lbl 410 `"Taiwanese"', add
label define raced_lbl 420 `"Chinese and Taiwanese"', add
label define raced_lbl 500 `"Japanese"', add
label define raced_lbl 600 `"Filipino"', add
label define raced_lbl 610 `"Asian Indian (Hindu 1920_1940)"', add
label define raced_lbl 620 `"Korean"', add
label define raced_lbl 630 `"Hawaiian"', add
label define raced_lbl 631 `"Hawaiian and Asian (1900,1920)"', add
label define raced_lbl 632 `"Hawaiian and European (1900,1920)"', add
label define raced_lbl 634 `"Hawaiian mixed"', add
label define raced_lbl 640 `"Vietnamese"', add
label define raced_lbl 641 `"   Bhutanese"', add
label define raced_lbl 642 `"   Mongolian "', add
label define raced_lbl 643 `"   Nepalese"', add
label define raced_lbl 650 `"Other Asian or Pacific Islander (1920,1980)"', add
label define raced_lbl 651 `"Asian only (CPS)"', add
label define raced_lbl 652 `"Pacific Islander only (CPS)"', add
label define raced_lbl 653 `"Asian or Pacific Islander, n.s. (1990 Internal Census files)"', add
label define raced_lbl 660 `"Cambodian"', add
label define raced_lbl 661 `"Hmong"', add
label define raced_lbl 662 `"Laotian"', add
label define raced_lbl 663 `"Thai"', add
label define raced_lbl 664 `"Bangladeshi"', add
label define raced_lbl 665 `"Burmese"', add
label define raced_lbl 666 `"Indonesian"', add
label define raced_lbl 667 `"Malaysian"', add
label define raced_lbl 668 `"Okinawan"', add
label define raced_lbl 669 `"Pakistani"', add
label define raced_lbl 670 `"Sri Lankan"', add
label define raced_lbl 671 `"Other Asian, n.e.c."', add
label define raced_lbl 672 `"Asian, not specified"', add
label define raced_lbl 673 `"Chinese and Japanese"', add
label define raced_lbl 674 `"Chinese and Filipino"', add
label define raced_lbl 675 `"Chinese and Vietnamese"', add
label define raced_lbl 676 `"Chinese and Asian write_in"', add
label define raced_lbl 677 `"Japanese and Filipino"', add
label define raced_lbl 678 `"Asian Indian and Asian write_in"', add
label define raced_lbl 679 `"Other Asian race combinations"', add
label define raced_lbl 680 `"Samoan"', add
label define raced_lbl 681 `"Tahitian"', add
label define raced_lbl 682 `"Tongan"', add
label define raced_lbl 683 `"Other Polynesian (1990)"', add
label define raced_lbl 684 `"1+ other Polynesian races (2000,ACS)"', add
label define raced_lbl 685 `"Guamanian/Chamorro"', add
label define raced_lbl 686 `"Northern Mariana Islander"', add
label define raced_lbl 687 `"Palauan"', add
label define raced_lbl 688 `"Other Micronesian (1990)"', add
label define raced_lbl 689 `"1+ other Micronesian races (2000,ACS)"', add
label define raced_lbl 690 `"Fijian"', add
label define raced_lbl 691 `"Other Melanesian (1990)"', add
label define raced_lbl 692 `"1+ other Melanesian races (2000,ACS)"', add
label define raced_lbl 698 `"2+ PI races from 2+ PI regions"', add
label define raced_lbl 699 `"Pacific Islander, n.s."', add
label define raced_lbl 700 `"Other race, n.e.c."', add
label define raced_lbl 801 `"White and Black"', add
label define raced_lbl 802 `"White and AIAN"', add
label define raced_lbl 810 `"White and Asian"', add
label define raced_lbl 811 `"White and Chinese"', add
label define raced_lbl 812 `"White and Japanese"', add
label define raced_lbl 813 `"White and Filipino"', add
label define raced_lbl 814 `"White and Asian Indian"', add
label define raced_lbl 815 `"White and Korean"', add
label define raced_lbl 816 `"White and Vietnamese"', add
label define raced_lbl 817 `"White and Asian write_in"', add
label define raced_lbl 818 `"White and other Asian race(s)"', add
label define raced_lbl 819 `"White and two or more Asian groups"', add
label define raced_lbl 820 `"White and PI  "', add
label define raced_lbl 821 `"White and Native Hawaiian"', add
label define raced_lbl 822 `"White and Samoan"', add
label define raced_lbl 823 `"White and Guamanian"', add
label define raced_lbl 824 `"White and PI write_in"', add
label define raced_lbl 825 `"White and other PI race(s)"', add
label define raced_lbl 826 `"White and other race write_in"', add
label define raced_lbl 827 `"White and other race, n.e.c."', add
label define raced_lbl 830 `"Black and AIAN"', add
label define raced_lbl 831 `"Black and Asian"', add
label define raced_lbl 832 `"Black and Chinese"', add
label define raced_lbl 833 `"Black and Japanese"', add
label define raced_lbl 834 `"Black and Filipino"', add
label define raced_lbl 835 `"Black and Asian Indian"', add
label define raced_lbl 836 `"Black and Korean"', add
label define raced_lbl 837 `"Black and Asian write_in"', add
label define raced_lbl 838 `"Black and other Asian race(s)"', add
label define raced_lbl 840 `"Black and PI"', add
label define raced_lbl 841 `"Black and PI write_in"', add
label define raced_lbl 842 `"Black and other PI race(s)"', add
label define raced_lbl 845 `"Black and other race write_in"', add
label define raced_lbl 850 `"AIAN and Asian"', add
label define raced_lbl 851 `"AIAN and Filipino (2000 1%)"', add
label define raced_lbl 852 `"AIAN and Asian Indian"', add
label define raced_lbl 853 `"AIAN and Asian write_in (2000 1%)"', add
label define raced_lbl 854 `"AIAN and other Asian race(s)"', add
label define raced_lbl 855 `"AIAN and PI"', add
label define raced_lbl 856 `"AIAN and other race write_in"', add
label define raced_lbl 860 `"Asian and PI"', add
label define raced_lbl 861 `"Chinese and Hawaiian"', add
label define raced_lbl 862 `"Chinese, Filipino, Hawaiian (2000 1%)"', add
label define raced_lbl 863 `"Japanese and Hawaiian (2000 1%)"', add
label define raced_lbl 864 `"Filipino and Hawaiian"', add
label define raced_lbl 865 `"Filipino and PI write_in"', add
label define raced_lbl 866 `"Asian Indian and PI write_in (2000 1%)"', add
label define raced_lbl 867 `"Asian write_in and PI write_in"', add
label define raced_lbl 868 `"Other Asian race(s) and PI race(s)"', add
label define raced_lbl 869 `"Japanese and Korean (ACS)"', add
label define raced_lbl 880 `"Asian and other race write_in"', add
label define raced_lbl 881 `"Chinese and other race write_in"', add
label define raced_lbl 882 `"Japanese and other race write_in"', add
label define raced_lbl 883 `"Filipino and other race write_in"', add
label define raced_lbl 884 `"Asian Indian and other race write_in"', add
label define raced_lbl 885 `"Asian write_in and other race write_in"', add
label define raced_lbl 886 `"Other Asian race(s) and other race write_in"', add
label define raced_lbl 887 `"      Chinese and Korean"', add
label define raced_lbl 890 `"PI and other race write_in: "', add
label define raced_lbl 891 `"PI write_in and other race write_in"', add
label define raced_lbl 892 `"Other PI race(s) and other race write_in"', add
label define raced_lbl 893 `"         Native Hawaiian or PI other race(s)"', add
label define raced_lbl 899 `"API and other race write_in"', add
label define raced_lbl 901 `"White, Black, AIAN"', add
label define raced_lbl 902 `"White, Black, Asian"', add
label define raced_lbl 903 `"White, Black, PI"', add
label define raced_lbl 904 `"White, Black, other race write_in"', add
label define raced_lbl 905 `"White, AIAN, Asian"', add
label define raced_lbl 906 `"White, AIAN, PI"', add
label define raced_lbl 907 `"White, AIAN, other race write_in"', add
label define raced_lbl 910 `"White, Asian, PI "', add
label define raced_lbl 911 `"White, Chinese, Hawaiian"', add
label define raced_lbl 912 `"White, Chinese, Filipino, Hawaiian (2000 1%)"', add
label define raced_lbl 913 `"White, Japanese, Hawaiian (2000 1%)"', add
label define raced_lbl 914 `"White, Filipino, Hawaiian"', add
label define raced_lbl 915 `"Other White, Asian race(s), PI race(s)"', add
label define raced_lbl 916 `"      White, AIAN and Filipino"', add
label define raced_lbl 917 `"      White, Black, and Filipino"', add
label define raced_lbl 920 `"White, Asian, other race write_in"', add
label define raced_lbl 921 `"White, Filipino, other race write_in (2000 1%)"', add
label define raced_lbl 922 `"White, Asian write_in, other race write_in (2000 1%)"', add
label define raced_lbl 923 `"Other White, Asian race(s), other race write_in (2000 1%)"', add
label define raced_lbl 925 `"White, PI, other race write_in"', add
label define raced_lbl 930 `"Black, AIAN, Asian"', add
label define raced_lbl 931 `"Black, AIAN, PI"', add
label define raced_lbl 932 `"Black, AIAN, other race write_in"', add
label define raced_lbl 933 `"Black, Asian, PI"', add
label define raced_lbl 934 `"Black, Asian, other race write_in"', add
label define raced_lbl 935 `"Black, PI, other race write_in"', add
label define raced_lbl 940 `"AIAN, Asian, PI"', add
label define raced_lbl 941 `"AIAN, Asian, other race write_in"', add
label define raced_lbl 942 `"AIAN, PI, other race write_in"', add
label define raced_lbl 943 `"Asian, PI, other race write_in"', add
label define raced_lbl 944 `"Asian (Chinese, Japanese, Korean, Vietnamese); and Native Hawaiian or PI; and Other"', add
label define raced_lbl 949 `"2 or 3 races (CPS)"', add
label define raced_lbl 950 `"White, Black, AIAN, Asian"', add
label define raced_lbl 951 `"White, Black, AIAN, PI"', add
label define raced_lbl 952 `"White, Black, AIAN, other race write_in"', add
label define raced_lbl 953 `"White, Black, Asian, PI"', add
label define raced_lbl 954 `"White, Black, Asian, other race write_in"', add
label define raced_lbl 955 `"White, Black, PI, other race write_in"', add
label define raced_lbl 960 `"White, AIAN, Asian, PI"', add
label define raced_lbl 961 `"White, AIAN, Asian, other race write_in"', add
label define raced_lbl 962 `"White, AIAN, PI, other race write_in"', add
label define raced_lbl 963 `"White, Asian, PI, other race write_in"', add
label define raced_lbl 964 `"White, Chinese, Japanese, Native Hawaiian"', add
label define raced_lbl 970 `"Black, AIAN, Asian, PI"', add
label define raced_lbl 971 `"Black, AIAN, Asian, other race write_in"', add
label define raced_lbl 972 `"Black, AIAN, PI, other race write_in"', add
label define raced_lbl 973 `"Black, Asian, PI, other race write_in"', add
label define raced_lbl 974 `"AIAN, Asian, PI, other race write_in"', add
label define raced_lbl 975 `"AIAN, Asian, PI, Hawaiian other race write_in"', add
label define raced_lbl 976 `"Two specified Asian  (Chinese and other Asian, Chinese and Japanese, Japanese and other Asian, Korean and other Asian); Native Hawaiian/PI; and Other Race"', add
label define raced_lbl 980 `"White, Black, AIAN, Asian, PI"', add
label define raced_lbl 981 `"White, Black, AIAN, Asian, other race write_in"', add
label define raced_lbl 982 `"White, Black, AIAN, PI, other race write_in"', add
label define raced_lbl 983 `"White, Black, Asian, PI, other race write_in"', add
label define raced_lbl 984 `"White, AIAN, Asian, PI, other race write_in"', add
label define raced_lbl 985 `"Black, AIAN, Asian, PI, other race write_in"', add
label define raced_lbl 986 `"Black, AIAN, Asian, PI, Hawaiian, other race write_in"', add
label define raced_lbl 989 `"4 or 5 races (CPS)"', add
label define raced_lbl 990 `"White, Black, AIAN, Asian, PI, other race write_in"', add
label define raced_lbl 991 `"White race; Some other race; Black or African American race and/or American Indian and Alaska Native race and/or Asian groups and/or Native Hawaiian and Other Pacific Islander groups"', add
label define raced_lbl 996 `"2+ races, n.e.c. (CPS)"', add
label values raced raced_lbl

label define bpl_lbl 001 `"Alabama"'
label define bpl_lbl 002 `"Alaska"', add
label define bpl_lbl 004 `"Arizona"', add
label define bpl_lbl 005 `"Arkansas"', add
label define bpl_lbl 006 `"California"', add
label define bpl_lbl 008 `"Colorado"', add
label define bpl_lbl 009 `"Connecticut"', add
label define bpl_lbl 010 `"Delaware"', add
label define bpl_lbl 011 `"District of Columbia"', add
label define bpl_lbl 012 `"Florida"', add
label define bpl_lbl 013 `"Georgia"', add
label define bpl_lbl 015 `"Hawaii"', add
label define bpl_lbl 016 `"Idaho"', add
label define bpl_lbl 017 `"Illinois"', add
label define bpl_lbl 018 `"Indiana"', add
label define bpl_lbl 019 `"Iowa"', add
label define bpl_lbl 020 `"Kansas"', add
label define bpl_lbl 021 `"Kentucky"', add
label define bpl_lbl 022 `"Louisiana"', add
label define bpl_lbl 023 `"Maine"', add
label define bpl_lbl 024 `"Maryland"', add
label define bpl_lbl 025 `"Massachusetts"', add
label define bpl_lbl 026 `"Michigan"', add
label define bpl_lbl 027 `"Minnesota"', add
label define bpl_lbl 028 `"Mississippi"', add
label define bpl_lbl 029 `"Missouri"', add
label define bpl_lbl 030 `"Montana"', add
label define bpl_lbl 031 `"Nebraska"', add
label define bpl_lbl 032 `"Nevada"', add
label define bpl_lbl 033 `"New Hampshire"', add
label define bpl_lbl 034 `"New Jersey"', add
label define bpl_lbl 035 `"New Mexico"', add
label define bpl_lbl 036 `"New York"', add
label define bpl_lbl 037 `"North Carolina"', add
label define bpl_lbl 038 `"North Dakota"', add
label define bpl_lbl 039 `"Ohio"', add
label define bpl_lbl 040 `"Oklahoma"', add
label define bpl_lbl 041 `"Oregon"', add
label define bpl_lbl 042 `"Pennsylvania"', add
label define bpl_lbl 044 `"Rhode Island"', add
label define bpl_lbl 045 `"South Carolina"', add
label define bpl_lbl 046 `"South Dakota"', add
label define bpl_lbl 047 `"Tennessee"', add
label define bpl_lbl 048 `"Texas"', add
label define bpl_lbl 049 `"Utah"', add
label define bpl_lbl 050 `"Vermont"', add
label define bpl_lbl 051 `"Virginia"', add
label define bpl_lbl 053 `"Washington"', add
label define bpl_lbl 054 `"West Virginia"', add
label define bpl_lbl 055 `"Wisconsin"', add
label define bpl_lbl 056 `"Wyoming"', add
label define bpl_lbl 090 `"Native American"', add
label define bpl_lbl 099 `"United States, ns"', add
label define bpl_lbl 100 `"American Samoa"', add
label define bpl_lbl 105 `"Guam"', add
label define bpl_lbl 110 `"Puerto Rico"', add
label define bpl_lbl 115 `"U.S. Virgin Islands"', add
label define bpl_lbl 120 `"Other US Possessions"', add
label define bpl_lbl 150 `"Canada"', add
label define bpl_lbl 155 `"St. Pierre and Miquelon"', add
label define bpl_lbl 160 `"Atlantic Islands"', add
label define bpl_lbl 199 `"North America, ns"', add
label define bpl_lbl 200 `"Mexico"', add
label define bpl_lbl 210 `"Central America"', add
label define bpl_lbl 250 `"Cuba"', add
label define bpl_lbl 260 `"West Indies"', add
label define bpl_lbl 299 `"Americas, n.s."', add
label define bpl_lbl 300 `"SOUTH AMERICA"', add
label define bpl_lbl 400 `"Denmark"', add
label define bpl_lbl 401 `"Finland"', add
label define bpl_lbl 402 `"Iceland"', add
label define bpl_lbl 403 `"Lapland, n.s."', add
label define bpl_lbl 404 `"Norway"', add
label define bpl_lbl 405 `"Sweden"', add
label define bpl_lbl 410 `"England"', add
label define bpl_lbl 411 `"Scotland"', add
label define bpl_lbl 412 `"Wales"', add
label define bpl_lbl 413 `"United Kingdom, ns"', add
label define bpl_lbl 414 `"Ireland"', add
label define bpl_lbl 419 `"Northern Europe, ns"', add
label define bpl_lbl 420 `"Belgium"', add
label define bpl_lbl 421 `"France"', add
label define bpl_lbl 422 `"Liechtenstein"', add
label define bpl_lbl 423 `"Luxembourg"', add
label define bpl_lbl 424 `"Monaco"', add
label define bpl_lbl 425 `"Netherlands"', add
label define bpl_lbl 426 `"Switzerland"', add
label define bpl_lbl 429 `"Western Europe, ns"', add
label define bpl_lbl 430 `"Albania"', add
label define bpl_lbl 431 `"Andorra"', add
label define bpl_lbl 432 `"Gibraltar"', add
label define bpl_lbl 433 `"Greece"', add
label define bpl_lbl 434 `"Italy"', add
label define bpl_lbl 435 `"Malta"', add
label define bpl_lbl 436 `"Portugal"', add
label define bpl_lbl 437 `"San Marino"', add
label define bpl_lbl 438 `"Spain"', add
label define bpl_lbl 439 `"Vatican City"', add
label define bpl_lbl 440 `"Southern Europe, ns"', add
label define bpl_lbl 450 `"Austria"', add
label define bpl_lbl 451 `"Bulgaria"', add
label define bpl_lbl 452 `"Czechoslovakia"', add
label define bpl_lbl 453 `"Germany"', add
label define bpl_lbl 454 `"Hungary"', add
label define bpl_lbl 455 `"Poland"', add
label define bpl_lbl 456 `"Romania"', add
label define bpl_lbl 457 `"Yugoslavia"', add
label define bpl_lbl 458 `"Central Europe, ns"', add
label define bpl_lbl 459 `"Eastern Europe, ns"', add
label define bpl_lbl 460 `"Estonia"', add
label define bpl_lbl 461 `"Latvia"', add
label define bpl_lbl 462 `"Lithuania"', add
label define bpl_lbl 463 `"Baltic States, ns"', add
label define bpl_lbl 465 `"Other USSR/Russia"', add
label define bpl_lbl 499 `"Europe, ns"', add
label define bpl_lbl 500 `"China"', add
label define bpl_lbl 501 `"Japan"', add
label define bpl_lbl 502 `"Korea"', add
label define bpl_lbl 509 `"East Asia, ns"', add
label define bpl_lbl 510 `"Brunei"', add
label define bpl_lbl 511 `"Cambodia (Kampuchea)"', add
label define bpl_lbl 512 `"Indonesia"', add
label define bpl_lbl 513 `"Laos"', add
label define bpl_lbl 514 `"Malaysia"', add
label define bpl_lbl 515 `"Philippines"', add
label define bpl_lbl 516 `"Singapore"', add
label define bpl_lbl 517 `"Thailand"', add
label define bpl_lbl 518 `"Vietnam"', add
label define bpl_lbl 519 `"Southeast Asia, ns"', add
label define bpl_lbl 520 `"Afghanistan"', add
label define bpl_lbl 521 `"India"', add
label define bpl_lbl 522 `"Iran"', add
label define bpl_lbl 523 `"Maldives"', add
label define bpl_lbl 524 `"Nepal"', add
label define bpl_lbl 530 `"Bahrain"', add
label define bpl_lbl 531 `"Cyprus"', add
label define bpl_lbl 532 `"Iraq"', add
label define bpl_lbl 533 `"Iraq/Saudi Arabia"', add
label define bpl_lbl 534 `"Israel/Palestine"', add
label define bpl_lbl 535 `"Jordan"', add
label define bpl_lbl 536 `"Kuwait"', add
label define bpl_lbl 537 `"Lebanon"', add
label define bpl_lbl 538 `"Oman"', add
label define bpl_lbl 539 `"Qatar"', add
label define bpl_lbl 540 `"Saudi Arabia"', add
label define bpl_lbl 541 `"Syria"', add
label define bpl_lbl 542 `"Turkey"', add
label define bpl_lbl 543 `"United Arab Emirates"', add
label define bpl_lbl 544 `"Yemen Arab Republic (North)"', add
label define bpl_lbl 545 `"Yemen, PDR (South)"', add
label define bpl_lbl 546 `"Persian Gulf States, n.s."', add
label define bpl_lbl 547 `"Middle East, ns"', add
label define bpl_lbl 548 `"Southwest Asia, nec/ns"', add
label define bpl_lbl 549 `"Asia Minor, ns"', add
label define bpl_lbl 550 `"South Asia, nec"', add
label define bpl_lbl 599 `"Asia, nec/ns"', add
label define bpl_lbl 600 `"AFRICA"', add
label define bpl_lbl 700 `"Australia and New Zealand"', add
label define bpl_lbl 710 `"Pacific Islands"', add
label define bpl_lbl 800 `"Antarctica, ns/nec"', add
label define bpl_lbl 900 `"Abroad (unknown) or at sea"', add
label define bpl_lbl 950 `"Other n.e.c."', add
label define bpl_lbl 999 `"Missing/blank"', add
label values bpl bpl_lbl

label define bpld_lbl 00100 `"Alabama"'
label define bpld_lbl 00200 `"Alaska"', add
label define bpld_lbl 00400 `"Arizona"', add
label define bpld_lbl 00500 `"Arkansas"', add
label define bpld_lbl 00600 `"California"', add
label define bpld_lbl 00800 `"Colorado"', add
label define bpld_lbl 00900 `"Connecticut"', add
label define bpld_lbl 01000 `"Delaware"', add
label define bpld_lbl 01100 `"District of Columbia"', add
label define bpld_lbl 01200 `"Florida"', add
label define bpld_lbl 01300 `"Georgia"', add
label define bpld_lbl 01500 `"Hawaii"', add
label define bpld_lbl 01600 `"Idaho"', add
label define bpld_lbl 01610 `"Idaho Territory"', add
label define bpld_lbl 01700 `"Illinois"', add
label define bpld_lbl 01800 `"Indiana"', add
label define bpld_lbl 01900 `"Iowa"', add
label define bpld_lbl 02000 `"Kansas"', add
label define bpld_lbl 02100 `"Kentucky"', add
label define bpld_lbl 02200 `"Louisiana"', add
label define bpld_lbl 02300 `"Maine"', add
label define bpld_lbl 02400 `"Maryland"', add
label define bpld_lbl 02500 `"Massachusetts"', add
label define bpld_lbl 02600 `"Michigan"', add
label define bpld_lbl 02700 `"Minnesota"', add
label define bpld_lbl 02800 `"Mississippi"', add
label define bpld_lbl 02900 `"Missouri"', add
label define bpld_lbl 03000 `"Montana"', add
label define bpld_lbl 03100 `"Nebraska"', add
label define bpld_lbl 03200 `"Nevada"', add
label define bpld_lbl 03300 `"New Hampshire"', add
label define bpld_lbl 03400 `"New Jersey"', add
label define bpld_lbl 03500 `"New Mexico"', add
label define bpld_lbl 03510 `"New Mexico Territory"', add
label define bpld_lbl 03600 `"New York"', add
label define bpld_lbl 03700 `"North Carolina"', add
label define bpld_lbl 03800 `"North Dakota"', add
label define bpld_lbl 03900 `"Ohio"', add
label define bpld_lbl 04000 `"Oklahoma"', add
label define bpld_lbl 04010 `"Indian Territory"', add
label define bpld_lbl 04100 `"Oregon"', add
label define bpld_lbl 04200 `"Pennsylvania"', add
label define bpld_lbl 04400 `"Rhode Island"', add
label define bpld_lbl 04500 `"South Carolina"', add
label define bpld_lbl 04600 `"South Dakota"', add
label define bpld_lbl 04610 `"Dakota Territory"', add
label define bpld_lbl 04700 `"Tennessee"', add
label define bpld_lbl 04800 `"Texas"', add
label define bpld_lbl 04900 `"Utah"', add
label define bpld_lbl 04910 `"Utah Territory"', add
label define bpld_lbl 05000 `"Vermont"', add
label define bpld_lbl 05100 `"Virginia"', add
label define bpld_lbl 05300 `"Washington"', add
label define bpld_lbl 05400 `"West Virginia"', add
label define bpld_lbl 05500 `"Wisconsin"', add
label define bpld_lbl 05600 `"Wyoming"', add
label define bpld_lbl 05610 `"Wyoming Territory"', add
label define bpld_lbl 09000 `"Native American"', add
label define bpld_lbl 09900 `"United States, ns"', add
label define bpld_lbl 10000 `"American Samoa"', add
label define bpld_lbl 10010 `"Samoa, 1940-1950"', add
label define bpld_lbl 10500 `"Guam"', add
label define bpld_lbl 11000 `"Puerto Rico"', add
label define bpld_lbl 11500 `"U.S. Virgin Islands"', add
label define bpld_lbl 11510 `"St. Croix"', add
label define bpld_lbl 11520 `"St. John"', add
label define bpld_lbl 11530 `"St. Thomas"', add
label define bpld_lbl 12000 `"Other US Possessions:"', add
label define bpld_lbl 12010 `"Johnston Atoll"', add
label define bpld_lbl 12020 `"Midway Islands"', add
label define bpld_lbl 12030 `"Wake Island"', add
label define bpld_lbl 12040 `"Other US Caribbean Islands"', add
label define bpld_lbl 12041 `"Navassa Island"', add
label define bpld_lbl 12050 `"Other US Pacific Islands"', add
label define bpld_lbl 12051 `"Baker Island"', add
label define bpld_lbl 12052 `"Howland Island"', add
label define bpld_lbl 12053 `"Jarvis Island"', add
label define bpld_lbl 12054 `"Kingman Reef"', add
label define bpld_lbl 12055 `"Palmyra Atoll"', add
label define bpld_lbl 12056 `"Canton and Enderbury Island"', add
label define bpld_lbl 12090 `"US outlying areas, ns"', add
label define bpld_lbl 12091 `"US possessions, ns"', add
label define bpld_lbl 12092 `"US territory, ns"', add
label define bpld_lbl 15000 `"Canada"', add
label define bpld_lbl 15010 `"English Canada"', add
label define bpld_lbl 15011 `"British Columbia"', add
label define bpld_lbl 15013 `"Alberta"', add
label define bpld_lbl 15015 `"Saskatchewan"', add
label define bpld_lbl 15017 `"Northwest"', add
label define bpld_lbl 15019 `"Ruperts Land"', add
label define bpld_lbl 15020 `"Manitoba"', add
label define bpld_lbl 15021 `"Red River"', add
label define bpld_lbl 15030 `"Ontario/Upper Canada"', add
label define bpld_lbl 15031 `"Upper Canada"', add
label define bpld_lbl 15032 `"Canada West"', add
label define bpld_lbl 15040 `"New Brunswick"', add
label define bpld_lbl 15050 `"Nova Scotia"', add
label define bpld_lbl 15051 `"Cape Breton"', add
label define bpld_lbl 15052 `"Halifax"', add
label define bpld_lbl 15060 `"Prince Edward Island"', add
label define bpld_lbl 15070 `"Newfoundland"', add
label define bpld_lbl 15080 `"French Canada"', add
label define bpld_lbl 15081 `"Quebec"', add
label define bpld_lbl 15082 `"Lower Canada"', add
label define bpld_lbl 15083 `"Canada East"', add
label define bpld_lbl 15500 `"St. Pierre and Miquelon"', add
label define bpld_lbl 16000 `"Atlantic Islands"', add
label define bpld_lbl 16010 `"Bermuda"', add
label define bpld_lbl 16020 `"Cape Verde"', add
label define bpld_lbl 16030 `"Falkland Islands"', add
label define bpld_lbl 16040 `"Greenland"', add
label define bpld_lbl 16050 `"St. Helena and Ascension"', add
label define bpld_lbl 16060 `"Canary Islands"', add
label define bpld_lbl 19900 `"North America, ns"', add
label define bpld_lbl 20000 `"Mexico"', add
label define bpld_lbl 21000 `"Central America"', add
label define bpld_lbl 21010 `"Belize/British Honduras"', add
label define bpld_lbl 21020 `"Costa Rica"', add
label define bpld_lbl 21030 `"El Salvador"', add
label define bpld_lbl 21040 `"Guatemala"', add
label define bpld_lbl 21050 `"Honduras"', add
label define bpld_lbl 21060 `"Nicaragua"', add
label define bpld_lbl 21070 `"Panama"', add
label define bpld_lbl 21071 `"Canal Zone"', add
label define bpld_lbl 21090 `"Central America, ns"', add
label define bpld_lbl 25000 `"Cuba"', add
label define bpld_lbl 26000 `"West Indies"', add
label define bpld_lbl 26010 `"Dominican Republic"', add
label define bpld_lbl 26020 `"Haiti"', add
label define bpld_lbl 26030 `"Jamaica"', add
label define bpld_lbl 26040 `"British West Indies"', add
label define bpld_lbl 26041 `"Anguilla"', add
label define bpld_lbl 26042 `"Antigua-Barbuda"', add
label define bpld_lbl 26043 `"Bahamas"', add
label define bpld_lbl 26044 `"Barbados"', add
label define bpld_lbl 26045 `"British Virgin Islands"', add
label define bpld_lbl 26046 `"Anegada"', add
label define bpld_lbl 26047 `"Cooper"', add
label define bpld_lbl 26048 `"Jost Van Dyke"', add
label define bpld_lbl 26049 `"Peter"', add
label define bpld_lbl 26050 `"Tortola"', add
label define bpld_lbl 26051 `"Virgin Gorda"', add
label define bpld_lbl 26052 `"Br. Virgin Islands, ns"', add
label define bpld_lbl 26053 `"Cayman Islands"', add
label define bpld_lbl 26054 `"Dominica"', add
label define bpld_lbl 26055 `"Grenada"', add
label define bpld_lbl 26056 `"Montserrat"', add
label define bpld_lbl 26057 `"St. Kitts-Nevis"', add
label define bpld_lbl 26058 `"St. Lucia"', add
label define bpld_lbl 26059 `"St. Vincent"', add
label define bpld_lbl 26060 `"Trinidad and Tobago"', add
label define bpld_lbl 26061 `"Turks and Caicos"', add
label define bpld_lbl 26069 `"Br. Virgin Islands, ns"', add
label define bpld_lbl 26070 `"Other West Indies"', add
label define bpld_lbl 26071 `"Aruba"', add
label define bpld_lbl 26072 `"Netherlands Antilles"', add
label define bpld_lbl 26073 `"Bonaire"', add
label define bpld_lbl 26074 `"Curacao"', add
label define bpld_lbl 26075 `"Dutch St. Maarten"', add
label define bpld_lbl 26076 `"Saba"', add
label define bpld_lbl 26077 `"St. Eustatius"', add
label define bpld_lbl 26079 `"Dutch Caribbean, ns"', add
label define bpld_lbl 26080 `"French St. Maarten"', add
label define bpld_lbl 26081 `"Guadeloupe"', add
label define bpld_lbl 26082 `"Martinique"', add
label define bpld_lbl 26083 `"St. Barthelemy"', add
label define bpld_lbl 26089 `"French Caribbean, ns"', add
label define bpld_lbl 26090 `"Antilles, ns"', add
label define bpld_lbl 26091 `"Caribbean, ns"', add
label define bpld_lbl 26092 `"Latin America, ns"', add
label define bpld_lbl 26093 `"Leeward Islands, ns"', add
label define bpld_lbl 26094 `"West Indies, ns"', add
label define bpld_lbl 26095 `"Windward Islands, ns"', add
label define bpld_lbl 29900 `"Americas, ns"', add
label define bpld_lbl 30000 `"South America"', add
label define bpld_lbl 30005 `"Argentina"', add
label define bpld_lbl 30010 `"Bolivia"', add
label define bpld_lbl 30015 `"Brazil"', add
label define bpld_lbl 30020 `"Chile"', add
label define bpld_lbl 30025 `"Colombia"', add
label define bpld_lbl 30030 `"Ecuador"', add
label define bpld_lbl 30035 `"French Guiana"', add
label define bpld_lbl 30040 `"Guyana/British Guiana"', add
label define bpld_lbl 30045 `"Paraguay"', add
label define bpld_lbl 30050 `"Peru"', add
label define bpld_lbl 30055 `"Suriname"', add
label define bpld_lbl 30060 `"Uruguay"', add
label define bpld_lbl 30065 `"Venezuela"', add
label define bpld_lbl 30090 `"South America, ns"', add
label define bpld_lbl 30091 `"South and Central America, n.s."', add
label define bpld_lbl 40000 `"Denmark"', add
label define bpld_lbl 40010 `"Faeroe Islands"', add
label define bpld_lbl 40100 `"Finland"', add
label define bpld_lbl 40200 `"Iceland"', add
label define bpld_lbl 40300 `"Lapland, ns"', add
label define bpld_lbl 40400 `"Norway"', add
label define bpld_lbl 40410 `"Svalbard and Jan Meyen"', add
label define bpld_lbl 40411 `"Svalbard"', add
label define bpld_lbl 40412 `"Jan Meyen"', add
label define bpld_lbl 40500 `"Sweden"', add
label define bpld_lbl 41000 `"England"', add
label define bpld_lbl 41010 `"Channel Islands"', add
label define bpld_lbl 41011 `"Guernsey"', add
label define bpld_lbl 41012 `"Jersey"', add
label define bpld_lbl 41020 `"Isle of Man"', add
label define bpld_lbl 41100 `"Scotland"', add
label define bpld_lbl 41200 `"Wales"', add
label define bpld_lbl 41300 `"United Kingdom, ns"', add
label define bpld_lbl 41400 `"Ireland"', add
label define bpld_lbl 41410 `"Northern Ireland"', add
label define bpld_lbl 41900 `"Northern Europe, ns"', add
label define bpld_lbl 42000 `"Belgium"', add
label define bpld_lbl 42100 `"France"', add
label define bpld_lbl 42110 `"Alsace-Lorraine"', add
label define bpld_lbl 42111 `"Alsace"', add
label define bpld_lbl 42112 `"Lorraine"', add
label define bpld_lbl 42200 `"Liechtenstein"', add
label define bpld_lbl 42300 `"Luxembourg"', add
label define bpld_lbl 42400 `"Monaco"', add
label define bpld_lbl 42500 `"Netherlands"', add
label define bpld_lbl 42600 `"Switzerland"', add
label define bpld_lbl 42900 `"Western Europe, ns"', add
label define bpld_lbl 43000 `"Albania"', add
label define bpld_lbl 43100 `"Andorra"', add
label define bpld_lbl 43200 `"Gibraltar"', add
label define bpld_lbl 43300 `"Greece"', add
label define bpld_lbl 43310 `"Dodecanese Islands"', add
label define bpld_lbl 43320 `"Turkey Greece"', add
label define bpld_lbl 43330 `"Macedonia"', add
label define bpld_lbl 43400 `"Italy"', add
label define bpld_lbl 43500 `"Malta"', add
label define bpld_lbl 43600 `"Portugal"', add
label define bpld_lbl 43610 `"Azores"', add
label define bpld_lbl 43620 `"Madeira Islands"', add
label define bpld_lbl 43630 `"Cape Verde Islands"', add
label define bpld_lbl 43640 `"St. Miguel"', add
label define bpld_lbl 43700 `"San Marino"', add
label define bpld_lbl 43800 `"Spain"', add
label define bpld_lbl 43900 `"Vatican City"', add
label define bpld_lbl 44000 `"Southern Europe, ns"', add
label define bpld_lbl 45000 `"Austria"', add
label define bpld_lbl 45010 `"Austria-Hungary"', add
label define bpld_lbl 45020 `"Austria-Graz"', add
label define bpld_lbl 45030 `"Austria-Linz"', add
label define bpld_lbl 45040 `"Austria-Salzburg"', add
label define bpld_lbl 45050 `"Austria-Tyrol"', add
label define bpld_lbl 45060 `"Austria-Vienna"', add
label define bpld_lbl 45070 `"Austria-Kaernsten"', add
label define bpld_lbl 45080 `"Austria-Neustadt"', add
label define bpld_lbl 45100 `"Bulgaria"', add
label define bpld_lbl 45200 `"Czechoslovakia"', add
label define bpld_lbl 45210 `"Bohemia"', add
label define bpld_lbl 45211 `"Bohemia-Moravia"', add
label define bpld_lbl 45212 `"Slovakia"', add
label define bpld_lbl 45213 `"Czech Republic"', add
label define bpld_lbl 45300 `"Germany"', add
label define bpld_lbl 45301 `"Berlin"', add
label define bpld_lbl 45302 `"West Berlin"', add
label define bpld_lbl 45303 `"East Berlin"', add
label define bpld_lbl 45310 `"West Germany"', add
label define bpld_lbl 45311 `"Baden"', add
label define bpld_lbl 45312 `"Bavaria"', add
label define bpld_lbl 45313 `"Braunschweig"', add
label define bpld_lbl 45314 `"Bremen"', add
label define bpld_lbl 45315 `"Hamburg"', add
label define bpld_lbl 45316 `"Hanover"', add
label define bpld_lbl 45317 `"Hessen"', add
label define bpld_lbl 45318 `"Hesse-Nassau"', add
label define bpld_lbl 45319 `"Lippe"', add
label define bpld_lbl 45320 `"Lubeck"', add
label define bpld_lbl 45321 `"Oldenburg"', add
label define bpld_lbl 45322 `"Rheinland"', add
label define bpld_lbl 45323 `"Schaumburg-Lippe"', add
label define bpld_lbl 45324 `"Schleswig"', add
label define bpld_lbl 45325 `"Sigmaringen"', add
label define bpld_lbl 45326 `"Schwarzburg"', add
label define bpld_lbl 45327 `"Westphalia"', add
label define bpld_lbl 45328 `"Wurttemberg"', add
label define bpld_lbl 45329 `"Waldeck"', add
label define bpld_lbl 45330 `"Wittenberg"', add
label define bpld_lbl 45331 `"Frankfurt"', add
label define bpld_lbl 45332 `"Saarland"', add
label define bpld_lbl 45333 `"Nordrhein-Westfalen"', add
label define bpld_lbl 45340 `"East Germany"', add
label define bpld_lbl 45341 `"Anhalt"', add
label define bpld_lbl 45342 `"Brandenburg"', add
label define bpld_lbl 45344 `"Kingdom of Saxony"', add
label define bpld_lbl 45345 `"Mecklenburg"', add
label define bpld_lbl 45346 `"Saxony"', add
label define bpld_lbl 45347 `"Thuringian States"', add
label define bpld_lbl 45348 `"Sachsen-Meiningen"', add
label define bpld_lbl 45349 `"Sachsen-Weimar-Eisenach"', add
label define bpld_lbl 45350 `"Probable Saxony"', add
label define bpld_lbl 45351 `"Schwerin"', add
label define bpld_lbl 45352 `"Strelitz"', add
label define bpld_lbl 45353 `"Probably Thuringian States"', add
label define bpld_lbl 45360 `"Prussia, nec"', add
label define bpld_lbl 45361 `"Hohenzollern"', add
label define bpld_lbl 45362 `"Niedersachsen"', add
label define bpld_lbl 45400 `"Hungary"', add
label define bpld_lbl 45500 `"Poland"', add
label define bpld_lbl 45510 `"Austrian Poland"', add
label define bpld_lbl 45511 `"Galicia"', add
label define bpld_lbl 45520 `"German Poland"', add
label define bpld_lbl 45521 `"East Prussia"', add
label define bpld_lbl 45522 `"Pomerania"', add
label define bpld_lbl 45523 `"Posen"', add
label define bpld_lbl 45524 `"Prussian Poland"', add
label define bpld_lbl 45525 `"Silesia"', add
label define bpld_lbl 45526 `"West Prussia"', add
label define bpld_lbl 45530 `"Russian Poland"', add
label define bpld_lbl 45600 `"Romania"', add
label define bpld_lbl 45610 `"Transylvania"', add
label define bpld_lbl 45700 `"Yugoslavia"', add
label define bpld_lbl 45710 `"Croatia"', add
label define bpld_lbl 45720 `"Montenegro"', add
label define bpld_lbl 45730 `"Serbia"', add
label define bpld_lbl 45740 `"Bosnia"', add
label define bpld_lbl 45750 `"Dalmatia"', add
label define bpld_lbl 45760 `"Slovonia"', add
label define bpld_lbl 45770 `"Carniola"', add
label define bpld_lbl 45780 `"Slovenia"', add
label define bpld_lbl 45790 `"Kosovo"', add
label define bpld_lbl 45800 `"Central Europe, ns"', add
label define bpld_lbl 45900 `"Eastern Europe, ns"', add
label define bpld_lbl 46000 `"Estonia"', add
label define bpld_lbl 46100 `"Latvia"', add
label define bpld_lbl 46200 `"Lithuania"', add
label define bpld_lbl 46300 `"Baltic States, ns"', add
label define bpld_lbl 46500 `"Other USSR/Russia"', add
label define bpld_lbl 46510 `"Byelorussia"', add
label define bpld_lbl 46520 `"Moldavia"', add
label define bpld_lbl 46521 `"Bessarabia"', add
label define bpld_lbl 46530 `"Ukraine"', add
label define bpld_lbl 46540 `"Armenia"', add
label define bpld_lbl 46541 `"Azerbaijan"', add
label define bpld_lbl 46542 `"Republic of Georgia"', add
label define bpld_lbl 46543 `"Kazakhstan"', add
label define bpld_lbl 46544 `"Kirghizia"', add
label define bpld_lbl 46545 `"Tadzhik"', add
label define bpld_lbl 46546 `"Turkmenistan"', add
label define bpld_lbl 46547 `"Uzbekistan"', add
label define bpld_lbl 46548 `"Siberia"', add
label define bpld_lbl 46590 `"USSR, ns"', add
label define bpld_lbl 49900 `"Europe, ns."', add
label define bpld_lbl 50000 `"China"', add
label define bpld_lbl 50010 `"Hong Kong"', add
label define bpld_lbl 50020 `"Macau"', add
label define bpld_lbl 50030 `"Mongolia"', add
label define bpld_lbl 50040 `"Taiwan"', add
label define bpld_lbl 50100 `"Japan"', add
label define bpld_lbl 50200 `"Korea"', add
label define bpld_lbl 50210 `"North Korea"', add
label define bpld_lbl 50220 `"South Korea"', add
label define bpld_lbl 50900 `"East Asia, ns"', add
label define bpld_lbl 51000 `"Brunei"', add
label define bpld_lbl 51100 `"Cambodia (Kampuchea)"', add
label define bpld_lbl 51200 `"Indonesia"', add
label define bpld_lbl 51210 `"East Indies"', add
label define bpld_lbl 51220 `"East Timor"', add
label define bpld_lbl 51300 `"Laos"', add
label define bpld_lbl 51400 `"Malaysia"', add
label define bpld_lbl 51500 `"Philippines"', add
label define bpld_lbl 51600 `"Singapore"', add
label define bpld_lbl 51700 `"Thailand"', add
label define bpld_lbl 51800 `"Vietnam"', add
label define bpld_lbl 51900 `"Southeast Asia, ns"', add
label define bpld_lbl 51910 `"Indochina, ns"', add
label define bpld_lbl 52000 `"Afghanistan"', add
label define bpld_lbl 52100 `"India"', add
label define bpld_lbl 52110 `"Bangladesh"', add
label define bpld_lbl 52120 `"Bhutan"', add
label define bpld_lbl 52130 `"Burma (Myanmar)"', add
label define bpld_lbl 52140 `"Pakistan"', add
label define bpld_lbl 52150 `"Sri Lanka (Ceylon)"', add
label define bpld_lbl 52200 `"Iran"', add
label define bpld_lbl 52300 `"Maldives"', add
label define bpld_lbl 52400 `"Nepal"', add
label define bpld_lbl 53000 `"Bahrain"', add
label define bpld_lbl 53100 `"Cyprus"', add
label define bpld_lbl 53200 `"Iraq"', add
label define bpld_lbl 53210 `"Mesopotamia"', add
label define bpld_lbl 53300 `"Iraq/Saudi Arabia"', add
label define bpld_lbl 53400 `"Israel/Palestine"', add
label define bpld_lbl 53410 `"Gaza Strip"', add
label define bpld_lbl 53420 `"Palestine"', add
label define bpld_lbl 53430 `"West Bank"', add
label define bpld_lbl 53440 `"Israel"', add
label define bpld_lbl 53500 `"Jordan"', add
label define bpld_lbl 53600 `"Kuwait"', add
label define bpld_lbl 53700 `"Lebanon"', add
label define bpld_lbl 53800 `"Oman"', add
label define bpld_lbl 53900 `"Qatar"', add
label define bpld_lbl 54000 `"Saudi Arabia"', add
label define bpld_lbl 54100 `"Syria"', add
label define bpld_lbl 54200 `"Turkey"', add
label define bpld_lbl 54210 `"European Turkey"', add
label define bpld_lbl 54220 `"Asian Turkey"', add
label define bpld_lbl 54300 `"United Arab Emirates"', add
label define bpld_lbl 54400 `"Yemen Arab Republic (North)"', add
label define bpld_lbl 54500 `"Yemen, PDR (South)"', add
label define bpld_lbl 54600 `"Persian Gulf States, ns"', add
label define bpld_lbl 54700 `"Middle East, ns"', add
label define bpld_lbl 54800 `"Southwest Asia, nec/ns"', add
label define bpld_lbl 54900 `"Asia Minor, ns"', add
label define bpld_lbl 55000 `"South Asia, nec"', add
label define bpld_lbl 59900 `"Asia, nec/ns"', add
label define bpld_lbl 60000 `"Africa"', add
label define bpld_lbl 60010 `"Northern Africa"', add
label define bpld_lbl 60011 `"Algeria"', add
label define bpld_lbl 60012 `"Egypt/United Arab Rep."', add
label define bpld_lbl 60013 `"Libya"', add
label define bpld_lbl 60014 `"Morocco"', add
label define bpld_lbl 60015 `"Sudan"', add
label define bpld_lbl 60016 `"Tunisia"', add
label define bpld_lbl 60017 `"Western Sahara"', add
label define bpld_lbl 60019 `"North Africa, ns"', add
label define bpld_lbl 60020 `"Benin"', add
label define bpld_lbl 60021 `"Burkina Faso"', add
label define bpld_lbl 60022 `"Gambia"', add
label define bpld_lbl 60023 `"Ghana"', add
label define bpld_lbl 60024 `"Guinea"', add
label define bpld_lbl 60025 `"Guinea-Bissau"', add
label define bpld_lbl 60026 `"Ivory Coast"', add
label define bpld_lbl 60027 `"Liberia"', add
label define bpld_lbl 60028 `"Mali"', add
label define bpld_lbl 60029 `"Mauritania"', add
label define bpld_lbl 60030 `"Niger"', add
label define bpld_lbl 60031 `"Nigeria"', add
label define bpld_lbl 60032 `"Senegal"', add
label define bpld_lbl 60033 `"Sierra Leone"', add
label define bpld_lbl 60034 `"Togo"', add
label define bpld_lbl 60038 `"Western Africa, ns"', add
label define bpld_lbl 60039 `"French West Africa, ns"', add
label define bpld_lbl 60040 `"British Indian Ocean Territory"', add
label define bpld_lbl 60041 `"Burundi"', add
label define bpld_lbl 60042 `"Comoros"', add
label define bpld_lbl 60043 `"Djibouti"', add
label define bpld_lbl 60044 `"Ethiopia"', add
label define bpld_lbl 60045 `"Kenya"', add
label define bpld_lbl 60046 `"Madagascar"', add
label define bpld_lbl 60047 `"Malawi"', add
label define bpld_lbl 60048 `"Mauritius"', add
label define bpld_lbl 60049 `"Mozambique"', add
label define bpld_lbl 60050 `"Reunion"', add
label define bpld_lbl 60051 `"Rwanda"', add
label define bpld_lbl 60052 `"Seychelles"', add
label define bpld_lbl 60053 `"Somalia"', add
label define bpld_lbl 60054 `"Tanzania"', add
label define bpld_lbl 60055 `"Uganda"', add
label define bpld_lbl 60056 `"Zambia"', add
label define bpld_lbl 60057 `"Zimbabwe"', add
label define bpld_lbl 60058 `"Bassas de India"', add
label define bpld_lbl 60059 `"Europa"', add
label define bpld_lbl 60060 `"Gloriosos"', add
label define bpld_lbl 60061 `"Juan de Nova"', add
label define bpld_lbl 60062 `"Mayotte"', add
label define bpld_lbl 60063 `"Tromelin"', add
label define bpld_lbl 60064 `"Eastern Africa, nec/ns"', add
label define bpld_lbl 60065 `"Eritrea"', add
label define bpld_lbl 60070 `"Central Africa"', add
label define bpld_lbl 60071 `"Angola"', add
label define bpld_lbl 60072 `"Cameroon"', add
label define bpld_lbl 60073 `"Central African Republic"', add
label define bpld_lbl 60074 `"Chad"', add
label define bpld_lbl 60075 `"Congo"', add
label define bpld_lbl 60076 `"Equatorial Guinea"', add
label define bpld_lbl 60077 `"Gabon"', add
label define bpld_lbl 60078 `"Sao Tome and Principe"', add
label define bpld_lbl 60079 `"Zaire"', add
label define bpld_lbl 60080 `"Central Africa, ns"', add
label define bpld_lbl 60081 `"Equatorial Africa, ns"', add
label define bpld_lbl 60082 `"French Equatorial Africa, ns"', add
label define bpld_lbl 60090 `"Southern Africa"', add
label define bpld_lbl 60091 `"Botswana"', add
label define bpld_lbl 60092 `"Lesotho"', add
label define bpld_lbl 60093 `"Namibia"', add
label define bpld_lbl 60094 `"South Africa (Union of)"', add
label define bpld_lbl 60095 `"Swaziland"', add
label define bpld_lbl 60096 `"Southern Africa, ns"', add
label define bpld_lbl 60099 `"Africa, ns/nec"', add
label define bpld_lbl 70000 `"Australia and New Zealand"', add
label define bpld_lbl 70010 `"Australia"', add
label define bpld_lbl 70011 `"Ashmore and Cartier Islands"', add
label define bpld_lbl 70012 `"Coral Sea Islands Territory"', add
label define bpld_lbl 70013 `"Christmas Island"', add
label define bpld_lbl 70014 `"Cocos Islands"', add
label define bpld_lbl 70020 `"New Zealand"', add
label define bpld_lbl 71000 `"Pacific Islands"', add
label define bpld_lbl 71010 `"New Caledonia"', add
label define bpld_lbl 71012 `"Papua New Guinea"', add
label define bpld_lbl 71013 `"Solomon Islands"', add
label define bpld_lbl 71014 `"Vanuatu (New Hebrides)"', add
label define bpld_lbl 71015 `"Fiji"', add
label define bpld_lbl 71016 `"Melanesia, ns"', add
label define bpld_lbl 71017 `"Norfolk Islands"', add
label define bpld_lbl 71018 `"Niue"', add
label define bpld_lbl 71020 `"Cook Islands"', add
label define bpld_lbl 71022 `"French Polynesia"', add
label define bpld_lbl 71023 `"Tonga"', add
label define bpld_lbl 71024 `"Wallis and Futuna Islands"', add
label define bpld_lbl 71025 `"Western Samoa"', add
label define bpld_lbl 71026 `"Pitcairn Island"', add
label define bpld_lbl 71027 `"Tokelau"', add
label define bpld_lbl 71028 `"Tuvalu"', add
label define bpld_lbl 71029 `"Polynesia, ns"', add
label define bpld_lbl 71032 `"Kiribati"', add
label define bpld_lbl 71033 `"Canton and Enderbury"', add
label define bpld_lbl 71034 `"Nauru"', add
label define bpld_lbl 71039 `"Micronesia, ns"', add
label define bpld_lbl 71040 `"US Pacific Trust Territories"', add
label define bpld_lbl 71041 `"Marshall Islands"', add
label define bpld_lbl 71042 `"Micronesia"', add
label define bpld_lbl 71043 `"Kosrae"', add
label define bpld_lbl 71044 `"Pohnpei"', add
label define bpld_lbl 71045 `"Truk"', add
label define bpld_lbl 71046 `"Yap"', add
label define bpld_lbl 71047 `"Northern Mariana Islands"', add
label define bpld_lbl 71048 `"Palau"', add
label define bpld_lbl 71049 `"Pacific Trust Terr, ns"', add
label define bpld_lbl 71050 `"Clipperton Island"', add
label define bpld_lbl 71090 `"Oceania, ns/nec"', add
label define bpld_lbl 80000 `"Antarctica, ns/nec"', add
label define bpld_lbl 80010 `"Bouvet Islands"', add
label define bpld_lbl 80020 `"British Antarctic Terr."', add
label define bpld_lbl 80030 `"Dronning Maud Land"', add
label define bpld_lbl 80040 `"French Southern and Antarctic Lands"', add
label define bpld_lbl 80050 `"Heard and McDonald Islands"', add
label define bpld_lbl 90000 `"Abroad (unknown) or at sea"', add
label define bpld_lbl 90010 `"Abroad, ns"', add
label define bpld_lbl 90011 `"Abroad (US citizen)"', add
label define bpld_lbl 90020 `"At sea"', add
label define bpld_lbl 90021 `"At sea (US citizen)"', add
label define bpld_lbl 90022 `"At sea or abroad (U.S. citizen)"', add
label define bpld_lbl 95000 `"Other n.e.c."', add
label define bpld_lbl 99900 `"Missing/blank"', add
label values bpld bpld_lbl

label define citizen_lbl 0 `"N/A"'
label define citizen_lbl 1 `"Born abroad of American parents"', add
label define citizen_lbl 2 `"Naturalized citizen"', add
label define citizen_lbl 3 `"Not a citizen"', add
label define citizen_lbl 4 `"Not a citizen, but has received first papers"', add
label define citizen_lbl 5 `"Foreign born, citizenship status not reported"', add
label values citizen citizen_lbl

label define educ_lbl 00 `"N/A or no schooling"'
label define educ_lbl 01 `"Nursery school to grade 4"', add
label define educ_lbl 02 `"Grade 5, 6, 7, or 8"', add
label define educ_lbl 03 `"Grade 9"', add
label define educ_lbl 04 `"Grade 10"', add
label define educ_lbl 05 `"Grade 11"', add
label define educ_lbl 06 `"Grade 12"', add
label define educ_lbl 07 `"1 year of college"', add
label define educ_lbl 08 `"2 years of college"', add
label define educ_lbl 09 `"3 years of college"', add
label define educ_lbl 10 `"4 years of college"', add
label define educ_lbl 11 `"5+ years of college"', add
label values educ educ_lbl

label define educd_lbl 000 `"N/A or no schooling"'
label define educd_lbl 001 `"N/A"', add
label define educd_lbl 002 `"No schooling completed"', add
label define educd_lbl 010 `"Nursery school to grade 4"', add
label define educd_lbl 011 `"Nursery school, preschool"', add
label define educd_lbl 012 `"Kindergarten"', add
label define educd_lbl 013 `"Grade 1, 2, 3, or 4"', add
label define educd_lbl 014 `"Grade 1"', add
label define educd_lbl 015 `"Grade 2"', add
label define educd_lbl 016 `"Grade 3"', add
label define educd_lbl 017 `"Grade 4"', add
label define educd_lbl 020 `"Grade 5, 6, 7, or 8"', add
label define educd_lbl 021 `"Grade 5 or 6"', add
label define educd_lbl 022 `"Grade 5"', add
label define educd_lbl 023 `"Grade 6"', add
label define educd_lbl 024 `"Grade 7 or 8"', add
label define educd_lbl 025 `"Grade 7"', add
label define educd_lbl 026 `"Grade 8"', add
label define educd_lbl 030 `"Grade 9"', add
label define educd_lbl 040 `"Grade 10"', add
label define educd_lbl 050 `"Grade 11"', add
label define educd_lbl 060 `"Grade 12"', add
label define educd_lbl 061 `"12th grade, no diploma"', add
label define educd_lbl 062 `"High school graduate or GED"', add
label define educd_lbl 063 `"Regular high school diploma"', add
label define educd_lbl 064 `"GED or alternative credential"', add
label define educd_lbl 065 `"Some college, but less than 1 year"', add
label define educd_lbl 070 `"1 year of college"', add
label define educd_lbl 071 `"1 or more years of college credit, no degree"', add
label define educd_lbl 080 `"2 years of college"', add
label define educd_lbl 081 `"Associate's degree, type not specified"', add
label define educd_lbl 082 `"Associate's degree, occupational program"', add
label define educd_lbl 083 `"Associate's degree, academic program"', add
label define educd_lbl 090 `"3 years of college"', add
label define educd_lbl 100 `"4 years of college"', add
label define educd_lbl 101 `"Bachelor's degree"', add
label define educd_lbl 110 `"5+ years of college"', add
label define educd_lbl 111 `"6 years of college (6+ in 1960-1970)"', add
label define educd_lbl 112 `"7 years of college"', add
label define educd_lbl 113 `"8+ years of college"', add
label define educd_lbl 114 `"Master's degree"', add
label define educd_lbl 115 `"Professional degree beyond a bachelor's degree"', add
label define educd_lbl 116 `"Doctoral degree"', add
label define educd_lbl 999 `"Missing"', add
label values educd educd_lbl

label define schltype_lbl 0 `"N/A"'
label define schltype_lbl 1 `"Not enrolled"', add
label define schltype_lbl 2 `"Public school"', add
label define schltype_lbl 3 `"Private school (1960,1990-2000,ACS,PRCS)"', add
label define schltype_lbl 4 `"Church-related (1980)"', add
label define schltype_lbl 5 `"Parochial (1970)"', add
label define schltype_lbl 6 `"Other private, 1980"', add
label define schltype_lbl 7 `"Other private, 1970"', add
label values schltype schltype_lbl

label define degfield_lbl 00 `"N/A"'
label define degfield_lbl 11 `"Agriculture"', add
label define degfield_lbl 13 `"Environment and Natural Resources"', add
label define degfield_lbl 14 `"Architecture"', add
label define degfield_lbl 15 `"Area, Ethnic, and Civilization Studies"', add
label define degfield_lbl 19 `"Communications"', add
label define degfield_lbl 20 `"Communication Technologies"', add
label define degfield_lbl 21 `"Computer and Information Sciences"', add
label define degfield_lbl 22 `"Cosmetology Services and Culinary Arts"', add
label define degfield_lbl 23 `"Education Administration and Teaching"', add
label define degfield_lbl 24 `"Engineering"', add
label define degfield_lbl 25 `"Engineering Technologies"', add
label define degfield_lbl 26 `"Linguistics and Foreign Languages"', add
label define degfield_lbl 29 `"Family and Consumer Sciences"', add
label define degfield_lbl 32 `"Law"', add
label define degfield_lbl 33 `"English Language, Literature, and Composition"', add
label define degfield_lbl 34 `"Liberal Arts and Humanities"', add
label define degfield_lbl 35 `"Library Science"', add
label define degfield_lbl 36 `"Biology and Life Sciences"', add
label define degfield_lbl 37 `"Mathematics and Statistics"', add
label define degfield_lbl 38 `"Military Technologies"', add
label define degfield_lbl 40 `"Interdisciplinary and Multi-Disciplinary Studies (General)"', add
label define degfield_lbl 41 `"Physical Fitness, Parks, Recreation, and Leisure"', add
label define degfield_lbl 48 `"Philosophy and Religious Studies"', add
label define degfield_lbl 49 `"Theology and Religious Vocations"', add
label define degfield_lbl 50 `"Physical Sciences"', add
label define degfield_lbl 51 `"Nuclear, Industrial Radiology, and Biological Technologies"', add
label define degfield_lbl 52 `"Psychology"', add
label define degfield_lbl 53 `"Criminal Justice and Fire Protection"', add
label define degfield_lbl 54 `"Public Affairs, Policy, and Social Work"', add
label define degfield_lbl 55 `"Social Sciences"', add
label define degfield_lbl 56 `"Construction Services"', add
label define degfield_lbl 57 `"Electrical and Mechanic Repairs and Technologies"', add
label define degfield_lbl 58 `"Precision Production and Industrial Arts"', add
label define degfield_lbl 59 `"Transportation Sciences and Technologies"', add
label define degfield_lbl 60 `"Fine Arts"', add
label define degfield_lbl 61 `"Medical and Health Sciences and Services"', add
label define degfield_lbl 62 `"Business"', add
label define degfield_lbl 64 `"History"', add
label values degfield degfield_lbl

label define degfieldd_lbl 0000 `"N/A"'
label define degfieldd_lbl 1100 `"General Agriculture"', add
label define degfieldd_lbl 1101 `"Agriculture Production and Management"', add
label define degfieldd_lbl 1102 `"Agricultural Economics"', add
label define degfieldd_lbl 1103 `"Animal Sciences"', add
label define degfieldd_lbl 1104 `"Food Science"', add
label define degfieldd_lbl 1105 `"Plant Science and Agronomy"', add
label define degfieldd_lbl 1106 `"Soil Science"', add
label define degfieldd_lbl 1199 `"Miscellaneous Agriculture"', add
label define degfieldd_lbl 1300 `"Environment and Natural Resources"', add
label define degfieldd_lbl 1301 `"Environmental Science"', add
label define degfieldd_lbl 1302 `"Forestry"', add
label define degfieldd_lbl 1303 `"Natural Resources Management"', add
label define degfieldd_lbl 1401 `"Architecture"', add
label define degfieldd_lbl 1501 `"Area, Ethnic, and Civilization Studies"', add
label define degfieldd_lbl 1900 `"Communications"', add
label define degfieldd_lbl 1901 `"Communications"', add
label define degfieldd_lbl 1902 `"Journalism"', add
label define degfieldd_lbl 1903 `"Mass Media"', add
label define degfieldd_lbl 1904 `"Advertising and Public Relations"', add
label define degfieldd_lbl 2001 `"Communication Technologies"', add
label define degfieldd_lbl 2100 `"Computer and Information Systems"', add
label define degfieldd_lbl 2101 `"Computer Programming and Data Processing"', add
label define degfieldd_lbl 2102 `"Computer Science"', add
label define degfieldd_lbl 2105 `"Information Sciences"', add
label define degfieldd_lbl 2106 `"Computer Information Management and Security"', add
label define degfieldd_lbl 2107 `"Computer Networking and Telecommunications"', add
label define degfieldd_lbl 2201 `"Cosmetology Services and Culinary Arts"', add
label define degfieldd_lbl 2300 `"General Education"', add
label define degfieldd_lbl 2301 `"Educational Administration and Supervision"', add
label define degfieldd_lbl 2303 `"School Student Counseling"', add
label define degfieldd_lbl 2304 `"Elementary Education"', add
label define degfieldd_lbl 2305 `"Mathematics Teacher Education"', add
label define degfieldd_lbl 2306 `"Physical and Health Education Teaching"', add
label define degfieldd_lbl 2307 `"Early Childhood Education"', add
label define degfieldd_lbl 2308 `"Science  and Computer Teacher Education"', add
label define degfieldd_lbl 2309 `"Secondary Teacher Education"', add
label define degfieldd_lbl 2310 `"Special Needs Education"', add
label define degfieldd_lbl 2311 `"Social Science or History Teacher Education"', add
label define degfieldd_lbl 2312 `"Teacher Education:  Multiple Levels"', add
label define degfieldd_lbl 2313 `"Language and Drama Education"', add
label define degfieldd_lbl 2314 `"Art and Music Education"', add
label define degfieldd_lbl 2399 `"Miscellaneous Education"', add
label define degfieldd_lbl 2400 `"General Engineering"', add
label define degfieldd_lbl 2401 `"Aerospace Engineering"', add
label define degfieldd_lbl 2402 `"Biological Engineering"', add
label define degfieldd_lbl 2403 `"Architectural Engineering"', add
label define degfieldd_lbl 2404 `"Biomedical Engineering"', add
label define degfieldd_lbl 2405 `"Chemical Engineering"', add
label define degfieldd_lbl 2406 `"Civil Engineering"', add
label define degfieldd_lbl 2407 `"Computer Engineering"', add
label define degfieldd_lbl 2408 `"Electrical Engineering"', add
label define degfieldd_lbl 2409 `"Engineering Mechanics, Physics, and Science"', add
label define degfieldd_lbl 2410 `"Environmental Engineering"', add
label define degfieldd_lbl 2411 `"Geological and Geophysical Engineering"', add
label define degfieldd_lbl 2412 `"Industrial and Manufacturing Engineering"', add
label define degfieldd_lbl 2413 `"Materials Engineering and Materials Science"', add
label define degfieldd_lbl 2414 `"Mechanical Engineering"', add
label define degfieldd_lbl 2415 `"Metallurgical Engineering"', add
label define degfieldd_lbl 2416 `"Mining and Mineral Engineering"', add
label define degfieldd_lbl 2417 `"Naval Architecture and Marine Engineering"', add
label define degfieldd_lbl 2418 `"Nuclear Engineering"', add
label define degfieldd_lbl 2419 `"Petroleum Engineering"', add
label define degfieldd_lbl 2499 `"Miscellaneous Engineering"', add
label define degfieldd_lbl 2500 `"Engineering Technologies"', add
label define degfieldd_lbl 2501 `"Engineering and Industrial Management"', add
label define degfieldd_lbl 2502 `"Electrical Engineering Technology"', add
label define degfieldd_lbl 2503 `"Industrial Production Technologies"', add
label define degfieldd_lbl 2504 `"Mechanical Engineering Related Technologies"', add
label define degfieldd_lbl 2599 `"Miscellaneous Engineering Technologies"', add
label define degfieldd_lbl 2600 `"Linguistics and Foreign Languages"', add
label define degfieldd_lbl 2601 `"Linguistics and Comparative Language and Literature"', add
label define degfieldd_lbl 2602 `"French, German, Latin and Other Common Foreign Language Studies"', add
label define degfieldd_lbl 2603 `"Other Foreign Languages"', add
label define degfieldd_lbl 2901 `"Family and Consumer Sciences"', add
label define degfieldd_lbl 3200 `"Law"', add
label define degfieldd_lbl 3201 `"Court Reporting"', add
label define degfieldd_lbl 3202 `"Pre-Law and Legal Studies"', add
label define degfieldd_lbl 3300 `"English Language, Literature, and Composition"', add
label define degfieldd_lbl 3301 `"English Language and Literature"', add
label define degfieldd_lbl 3302 `"Composition and Speech"', add
label define degfieldd_lbl 3400 `"Liberal Arts and Humanities"', add
label define degfieldd_lbl 3401 `"Liberal Arts"', add
label define degfieldd_lbl 3402 `"Humanities"', add
label define degfieldd_lbl 3501 `"Library Science"', add
label define degfieldd_lbl 3600 `"Biology"', add
label define degfieldd_lbl 3601 `"Biochemical Sciences"', add
label define degfieldd_lbl 3602 `"Botany"', add
label define degfieldd_lbl 3603 `"Molecular Biology"', add
label define degfieldd_lbl 3604 `"Ecology"', add
label define degfieldd_lbl 3605 `"Genetics"', add
label define degfieldd_lbl 3606 `"Microbiology"', add
label define degfieldd_lbl 3607 `"Pharmacology"', add
label define degfieldd_lbl 3608 `"Physiology"', add
label define degfieldd_lbl 3609 `"Zoology"', add
label define degfieldd_lbl 3611 `"Neuroscience"', add
label define degfieldd_lbl 3699 `"Miscellaneous Biology"', add
label define degfieldd_lbl 3700 `"Mathematics"', add
label define degfieldd_lbl 3701 `"Applied Mathematics"', add
label define degfieldd_lbl 3702 `"Statistics and Decision Science"', add
label define degfieldd_lbl 3801 `"Military Technologies"', add
label define degfieldd_lbl 4000 `"Interdisciplinary and Multi-Disciplinary Studies (General)"', add
label define degfieldd_lbl 4001 `"Intercultural and International Studies"', add
label define degfieldd_lbl 4002 `"Nutrition Sciences"', add
label define degfieldd_lbl 4003 `"Neuroscience"', add
label define degfieldd_lbl 4005 `"Mathematics and Computer Science"', add
label define degfieldd_lbl 4006 `"Cognitive Science and Biopsychology"', add
label define degfieldd_lbl 4007 `"Interdisciplinary Social Sciences"', add
label define degfieldd_lbl 4008 `"Multi-disciplinary or General Science"', add
label define degfieldd_lbl 4101 `"Physical Fitness, Parks, Recreation, and Leisure"', add
label define degfieldd_lbl 4801 `"Philosophy and Religious Studies"', add
label define degfieldd_lbl 4901 `"Theology and Religious Vocations"', add
label define degfieldd_lbl 5000 `"Physical Sciences"', add
label define degfieldd_lbl 5001 `"Astronomy and Astrophysics"', add
label define degfieldd_lbl 5002 `"Atmospheric Sciences and Meteorology"', add
label define degfieldd_lbl 5003 `"Chemistry"', add
label define degfieldd_lbl 5004 `"Geology and Earth Science"', add
label define degfieldd_lbl 5005 `"Geosciences"', add
label define degfieldd_lbl 5006 `"Oceanography"', add
label define degfieldd_lbl 5007 `"Physics"', add
label define degfieldd_lbl 5008 `"Materials Science"', add
label define degfieldd_lbl 5098 `"Multi-disciplinary or General Science"', add
label define degfieldd_lbl 5102 `"Nuclear, Industrial Radiology, and Biological Technologies"', add
label define degfieldd_lbl 5200 `"Psychology"', add
label define degfieldd_lbl 5201 `"Educational Psychology"', add
label define degfieldd_lbl 5202 `"Clinical Psychology"', add
label define degfieldd_lbl 5203 `"Counseling Psychology"', add
label define degfieldd_lbl 5205 `"Industrial and Organizational Psychology"', add
label define degfieldd_lbl 5206 `"Social Psychology"', add
label define degfieldd_lbl 5299 `"Miscellaneous Psychology"', add
label define degfieldd_lbl 5301 `"Criminal Justice and Fire Protection"', add
label define degfieldd_lbl 5400 `"Public Affairs, Policy, and Social Work"', add
label define degfieldd_lbl 5401 `"Public Administration"', add
label define degfieldd_lbl 5402 `"Public Policy"', add
label define degfieldd_lbl 5403 `"Human Services and Community Organization"', add
label define degfieldd_lbl 5404 `"Social Work"', add
label define degfieldd_lbl 5500 `"General Social Sciences"', add
label define degfieldd_lbl 5501 `"Economics"', add
label define degfieldd_lbl 5502 `"Anthropology and Archeology"', add
label define degfieldd_lbl 5503 `"Criminology"', add
label define degfieldd_lbl 5504 `"Geography"', add
label define degfieldd_lbl 5505 `"International Relations"', add
label define degfieldd_lbl 5506 `"Political Science and Government"', add
label define degfieldd_lbl 5507 `"Sociology"', add
label define degfieldd_lbl 5599 `"Miscellaneous Social Sciences"', add
label define degfieldd_lbl 5601 `"Construction Services"', add
label define degfieldd_lbl 5701 `"Electrical and Mechanic Repairs and Technologies"', add
label define degfieldd_lbl 5801 `"Precision Production and Industrial Arts"', add
label define degfieldd_lbl 5901 `"Transportation Sciences and Technologies"', add
label define degfieldd_lbl 6000 `"Fine Arts"', add
label define degfieldd_lbl 6001 `"Drama and Theater Arts"', add
label define degfieldd_lbl 6002 `"Music"', add
label define degfieldd_lbl 6003 `"Visual and Performing Arts"', add
label define degfieldd_lbl 6004 `"Commercial Art and Graphic Design"', add
label define degfieldd_lbl 6005 `"Film, Video and Photographic Arts"', add
label define degfieldd_lbl 6006 `"Art History and Criticism"', add
label define degfieldd_lbl 6007 `"Studio Arts"', add
label define degfieldd_lbl 6099 `"Miscellaneous Fine Arts"', add
label define degfieldd_lbl 6100 `"General Medical and Health Services"', add
label define degfieldd_lbl 6102 `"Communication Disorders Sciences and Services"', add
label define degfieldd_lbl 6103 `"Health and Medical Administrative Services"', add
label define degfieldd_lbl 6104 `"Medical Assisting Services"', add
label define degfieldd_lbl 6105 `"Medical Technologies Technicians"', add
label define degfieldd_lbl 6106 `"Health and Medical Preparatory Programs"', add
label define degfieldd_lbl 6107 `"Nursing"', add
label define degfieldd_lbl 6108 `"Pharmacy, Pharmaceutical Sciences, and Administration"', add
label define degfieldd_lbl 6109 `"Treatment Therapy Professions"', add
label define degfieldd_lbl 6110 `"Community and Public Health"', add
label define degfieldd_lbl 6199 `"Miscellaneous Health Medical Professions"', add
label define degfieldd_lbl 6200 `"General Business"', add
label define degfieldd_lbl 6201 `"Accounting"', add
label define degfieldd_lbl 6202 `"Actuarial Science"', add
label define degfieldd_lbl 6203 `"Business Management and Administration"', add
label define degfieldd_lbl 6204 `"Operations, Logistics and E-Commerce"', add
label define degfieldd_lbl 6205 `"Business Economics"', add
label define degfieldd_lbl 6206 `"Marketing and Marketing Research"', add
label define degfieldd_lbl 6207 `"Finance"', add
label define degfieldd_lbl 6209 `"Human Resources and Personnel Management"', add
label define degfieldd_lbl 6210 `"International Business"', add
label define degfieldd_lbl 6211 `"Hospitality Management"', add
label define degfieldd_lbl 6212 `"Management Information Systems and Statistics"', add
label define degfieldd_lbl 6299 `"Miscellaneous Business and Medical Administration"', add
label define degfieldd_lbl 6402 `"History"', add
label define degfieldd_lbl 6403 `"United States History"', add
label values degfieldd degfieldd_lbl

label define degfield2_lbl 00 `"N/A"'
label define degfield2_lbl 11 `"Agriculture"', add
label define degfield2_lbl 13 `"Environment and Natural Resources"', add
label define degfield2_lbl 14 `"Architecture"', add
label define degfield2_lbl 15 `"Area, Ethnic, and Civilization Studies"', add
label define degfield2_lbl 19 `"Communications"', add
label define degfield2_lbl 20 `"Communication Technologies"', add
label define degfield2_lbl 21 `"Computer and Information Sciences"', add
label define degfield2_lbl 22 `"Cosmetology Services and Culinary Arts"', add
label define degfield2_lbl 23 `"Education Administration and Teaching"', add
label define degfield2_lbl 24 `"Engineering"', add
label define degfield2_lbl 25 `"Engineering Technologies"', add
label define degfield2_lbl 26 `"Linguistics and Foreign Languages"', add
label define degfield2_lbl 29 `"Family and Consumer Sciences"', add
label define degfield2_lbl 32 `"Law"', add
label define degfield2_lbl 33 `"English Language, Literature, and Composition"', add
label define degfield2_lbl 34 `"Liberal Arts and Humanities"', add
label define degfield2_lbl 35 `"Library Science"', add
label define degfield2_lbl 36 `"Biology and Life Sciences"', add
label define degfield2_lbl 37 `"Mathematics and Statistics"', add
label define degfield2_lbl 38 `"Military Technologies"', add
label define degfield2_lbl 40 `"Interdisciplinary and Multi-Disciplinary Studies (General)"', add
label define degfield2_lbl 41 `"Physical Fitness, Parks, Recreation, and Leisure"', add
label define degfield2_lbl 48 `"Philosophy and Religious Studies"', add
label define degfield2_lbl 49 `"Theology and Religious Vocations"', add
label define degfield2_lbl 50 `"Physical Sciences"', add
label define degfield2_lbl 51 `"Nuclear, Industrial Radiology, and Biological Technologies"', add
label define degfield2_lbl 52 `"Psychology"', add
label define degfield2_lbl 53 `"Criminal Justice and Fire Protection"', add
label define degfield2_lbl 54 `"Public Affairs, Policy, and Social Work"', add
label define degfield2_lbl 55 `"Social Sciences"', add
label define degfield2_lbl 56 `"Construction Services"', add
label define degfield2_lbl 57 `"Electrical and Mechanic Repairs and Technologies"', add
label define degfield2_lbl 58 `"Precision Production and Industrial Arts"', add
label define degfield2_lbl 59 `"Transportation Sciences and Technologies"', add
label define degfield2_lbl 60 `"Fine Arts"', add
label define degfield2_lbl 61 `"Medical and Health Sciences and Services"', add
label define degfield2_lbl 62 `"Business"', add
label define degfield2_lbl 64 `"History"', add
label values degfield2 degfield2_lbl

label define degfield2d_lbl 0000 `"N/A"'
label define degfield2d_lbl 1100 `"General Agriculture"', add
label define degfield2d_lbl 1101 `"Agriculture Production and Management"', add
label define degfield2d_lbl 1102 `"Agricultural Economics"', add
label define degfield2d_lbl 1103 `"Animal Sciences"', add
label define degfield2d_lbl 1104 `"Food Science"', add
label define degfield2d_lbl 1105 `"Plant Science and Agronomy"', add
label define degfield2d_lbl 1106 `"Soil Science"', add
label define degfield2d_lbl 1199 `"Miscellaneous Agriculture"', add
label define degfield2d_lbl 1300 `"Environment and Natural Resources"', add
label define degfield2d_lbl 1301 `"Environmental Science"', add
label define degfield2d_lbl 1302 `"Forestry"', add
label define degfield2d_lbl 1303 `"Natural Resources Management"', add
label define degfield2d_lbl 1401 `"Architecture"', add
label define degfield2d_lbl 1501 `"Area, Ethnic, and Civilization Studies"', add
label define degfield2d_lbl 1900 `"Communications"', add
label define degfield2d_lbl 1901 `"Communications"', add
label define degfield2d_lbl 1902 `"Journalism"', add
label define degfield2d_lbl 1903 `"Mass Media"', add
label define degfield2d_lbl 1904 `"Advertising and Public Relations"', add
label define degfield2d_lbl 2001 `"Communication Technologies"', add
label define degfield2d_lbl 2100 `"Computer and Information Systems"', add
label define degfield2d_lbl 2101 `"Computer Programming and Data Processing"', add
label define degfield2d_lbl 2102 `"Computer Science"', add
label define degfield2d_lbl 2105 `"Information Sciences"', add
label define degfield2d_lbl 2106 `"Computer Information Management and Security"', add
label define degfield2d_lbl 2107 `"Computer Networking and Telecommunications"', add
label define degfield2d_lbl 2201 `"Cosmetology Services and Culinary Arts"', add
label define degfield2d_lbl 2300 `"General Education"', add
label define degfield2d_lbl 2301 `"Educational Administration and Supervision"', add
label define degfield2d_lbl 2303 `"School Student Counseling"', add
label define degfield2d_lbl 2304 `"Elementary Education"', add
label define degfield2d_lbl 2305 `"Mathematics Teacher Education"', add
label define degfield2d_lbl 2306 `"Physical and Health Education Teaching"', add
label define degfield2d_lbl 2307 `"Early Childhood Education"', add
label define degfield2d_lbl 2308 `"Science  and Computer Teacher Education"', add
label define degfield2d_lbl 2309 `"Secondary Teacher Education"', add
label define degfield2d_lbl 2310 `"Special Needs Education"', add
label define degfield2d_lbl 2311 `"Social Science or History Teacher Education"', add
label define degfield2d_lbl 2312 `"Teacher Education:  Multiple Levels"', add
label define degfield2d_lbl 2313 `"Language and Drama Education"', add
label define degfield2d_lbl 2314 `"Art and Music Education"', add
label define degfield2d_lbl 2399 `"Miscellaneous Education"', add
label define degfield2d_lbl 2400 `"General Engineering"', add
label define degfield2d_lbl 2401 `"Aerospace Engineering"', add
label define degfield2d_lbl 2402 `"Biological Engineering"', add
label define degfield2d_lbl 2403 `"Architectural Engineering"', add
label define degfield2d_lbl 2404 `"Biomedical Engineering"', add
label define degfield2d_lbl 2405 `"Chemical Engineering"', add
label define degfield2d_lbl 2406 `"Civil Engineering"', add
label define degfield2d_lbl 2407 `"Computer Engineering"', add
label define degfield2d_lbl 2408 `"Electrical Engineering"', add
label define degfield2d_lbl 2409 `"Engineering Mechanics, Physics, and Science"', add
label define degfield2d_lbl 2410 `"Environmental Engineering"', add
label define degfield2d_lbl 2411 `"Geological and Geophysical Engineering"', add
label define degfield2d_lbl 2412 `"Industrial and Manufacturing Engineering"', add
label define degfield2d_lbl 2413 `"Materials Engineering and Materials Science"', add
label define degfield2d_lbl 2414 `"Mechanical Engineering"', add
label define degfield2d_lbl 2415 `"Metallurgical Engineering"', add
label define degfield2d_lbl 2416 `"Mining and Mineral Engineering"', add
label define degfield2d_lbl 2417 `"Naval Architecture and Marine Engineering"', add
label define degfield2d_lbl 2418 `"Nuclear Engineering"', add
label define degfield2d_lbl 2419 `"Petroleum Engineering"', add
label define degfield2d_lbl 2499 `"Miscellaneous Engineering"', add
label define degfield2d_lbl 2500 `"Engineering Technologies"', add
label define degfield2d_lbl 2501 `"Engineering and Industrial Management"', add
label define degfield2d_lbl 2502 `"Electrical Engineering Technology"', add
label define degfield2d_lbl 2503 `"Industrial Production Technologies"', add
label define degfield2d_lbl 2504 `"Mechanical Engineering Related Technologies"', add
label define degfield2d_lbl 2599 `"Miscellaneous Engineering Technologies"', add
label define degfield2d_lbl 2600 `"Linguistics and Foreign Languages"', add
label define degfield2d_lbl 2601 `"Linguistics and Comparative Language and Literature"', add
label define degfield2d_lbl 2602 `"French, German, Latin and Other Common Foreign Language Studies"', add
label define degfield2d_lbl 2603 `"Other Foreign Languages"', add
label define degfield2d_lbl 2901 `"Family and Consumer Sciences"', add
label define degfield2d_lbl 3200 `"Law"', add
label define degfield2d_lbl 3201 `"Court Reporting"', add
label define degfield2d_lbl 3202 `"Pre-Law and Legal Studies"', add
label define degfield2d_lbl 3300 `"English Language, Literature, and Composition"', add
label define degfield2d_lbl 3301 `"English Language and Literature"', add
label define degfield2d_lbl 3302 `"Composition and Speech"', add
label define degfield2d_lbl 3400 `"Liberal Arts and Humanities"', add
label define degfield2d_lbl 3401 `"Liberal Arts"', add
label define degfield2d_lbl 3402 `"Humanities"', add
label define degfield2d_lbl 3501 `"Library Science"', add
label define degfield2d_lbl 3600 `"Biology"', add
label define degfield2d_lbl 3601 `"Biochemical Sciences"', add
label define degfield2d_lbl 3602 `"Botany"', add
label define degfield2d_lbl 3603 `"Molecular Biology"', add
label define degfield2d_lbl 3604 `"Ecology"', add
label define degfield2d_lbl 3605 `"Genetics"', add
label define degfield2d_lbl 3606 `"Microbiology"', add
label define degfield2d_lbl 3607 `"Pharmacology"', add
label define degfield2d_lbl 3608 `"Physiology"', add
label define degfield2d_lbl 3609 `"Zoology"', add
label define degfield2d_lbl 3611 `"Neuroscience"', add
label define degfield2d_lbl 3699 `"Miscellaneous Biology"', add
label define degfield2d_lbl 3700 `"Mathematics"', add
label define degfield2d_lbl 3701 `"Applied Mathematics"', add
label define degfield2d_lbl 3702 `"Statistics and Decision Science"', add
label define degfield2d_lbl 3801 `"Military Technologies"', add
label define degfield2d_lbl 4000 `"Interdisciplinary and Multi-Disciplinary Studies (General)"', add
label define degfield2d_lbl 4001 `"Intercultural and International Studies"', add
label define degfield2d_lbl 4002 `"Nutrition Sciences"', add
label define degfield2d_lbl 4003 `"Neuroscience"', add
label define degfield2d_lbl 4004 `"Accounting and Computer Science"', add
label define degfield2d_lbl 4005 `"Mathematics and Computer Science"', add
label define degfield2d_lbl 4006 `"Cognitive Science and Biopsychology"', add
label define degfield2d_lbl 4007 `"Interdisciplinary Social Sciences"', add
label define degfield2d_lbl 4008 `"Multi-disciplinary or General Science"', add
label define degfield2d_lbl 4101 `"Physical Fitness, Parks, Recreation, and Leisure"', add
label define degfield2d_lbl 4801 `"Philosophy and Religious Studies"', add
label define degfield2d_lbl 4901 `"Theology and Religious Vocations"', add
label define degfield2d_lbl 5000 `"Physical Sciences"', add
label define degfield2d_lbl 5001 `"Astronomy and Astrophysics"', add
label define degfield2d_lbl 5002 `"Atmospheric Sciences and Meteorology"', add
label define degfield2d_lbl 5003 `"Chemistry"', add
label define degfield2d_lbl 5004 `"Geology and Earth Science"', add
label define degfield2d_lbl 5005 `"Geosciences"', add
label define degfield2d_lbl 5006 `"Oceanography"', add
label define degfield2d_lbl 5007 `"Physics"', add
label define degfield2d_lbl 5008 `"Materials Science"', add
label define degfield2d_lbl 5098 `"Multi-disciplinary or General Science"', add
label define degfield2d_lbl 5102 `"Nuclear, Industrial Radiology, and Biological Technologies"', add
label define degfield2d_lbl 5200 `"Psychology"', add
label define degfield2d_lbl 5201 `"Educational Psychology"', add
label define degfield2d_lbl 5202 `"Clinical Psychology"', add
label define degfield2d_lbl 5203 `"Counseling Psychology"', add
label define degfield2d_lbl 5205 `"Industrial and Organizational Psychology"', add
label define degfield2d_lbl 5206 `"Social Psychology"', add
label define degfield2d_lbl 5299 `"Miscellaneous Psychology"', add
label define degfield2d_lbl 5301 `"Criminal Justice and Fire Protection"', add
label define degfield2d_lbl 5400 `"Public Affairs, Policy, and Social Work"', add
label define degfield2d_lbl 5401 `"Public Administration"', add
label define degfield2d_lbl 5402 `"Public Policy"', add
label define degfield2d_lbl 5403 `"Human Services and Community Organization"', add
label define degfield2d_lbl 5404 `"Social Work"', add
label define degfield2d_lbl 5500 `"General Social Sciences"', add
label define degfield2d_lbl 5501 `"Economics"', add
label define degfield2d_lbl 5502 `"Anthropology and Archeology"', add
label define degfield2d_lbl 5503 `"Criminology"', add
label define degfield2d_lbl 5504 `"Geography"', add
label define degfield2d_lbl 5505 `"International Relations"', add
label define degfield2d_lbl 5506 `"Political Science and Government"', add
label define degfield2d_lbl 5507 `"Sociology"', add
label define degfield2d_lbl 5599 `"Miscellaneous Social Sciences"', add
label define degfield2d_lbl 5601 `"Construction Services"', add
label define degfield2d_lbl 5701 `"Electrical and Mechanic Repairs and Technologies"', add
label define degfield2d_lbl 5801 `"Precision Production and Industrial Arts"', add
label define degfield2d_lbl 5901 `"Transportation Sciences and Technologies"', add
label define degfield2d_lbl 6000 `"Fine Arts"', add
label define degfield2d_lbl 6001 `"Drama and Theater Arts"', add
label define degfield2d_lbl 6002 `"Music"', add
label define degfield2d_lbl 6003 `"Visual and Performing Arts"', add
label define degfield2d_lbl 6004 `"Commercial Art and Graphic Design"', add
label define degfield2d_lbl 6005 `"Film, Video and Photographic Arts"', add
label define degfield2d_lbl 6006 `"Art History and Criticism"', add
label define degfield2d_lbl 6007 `"Studio Arts"', add
label define degfield2d_lbl 6008 `"Video Game Design and Development"', add
label define degfield2d_lbl 6099 `"Miscellaneous Fine Arts"', add
label define degfield2d_lbl 6100 `"General Medical and Health Services"', add
label define degfield2d_lbl 6102 `"Communication Disorders Sciences and Services"', add
label define degfield2d_lbl 6103 `"Health and Medical Administrative Services"', add
label define degfield2d_lbl 6104 `"Medical Assisting Services"', add
label define degfield2d_lbl 6105 `"Medical Technologies Technicians"', add
label define degfield2d_lbl 6106 `"Health and Medical Preparatory Programs"', add
label define degfield2d_lbl 6107 `"Nursing"', add
label define degfield2d_lbl 6108 `"Pharmacy, Pharmaceutical Sciences, and Administration"', add
label define degfield2d_lbl 6109 `"Treatment Therapy Professions"', add
label define degfield2d_lbl 6110 `"Community and Public Health"', add
label define degfield2d_lbl 6199 `"Miscellaneous Health Medical Professions"', add
label define degfield2d_lbl 6200 `"General Business"', add
label define degfield2d_lbl 6201 `"Accounting"', add
label define degfield2d_lbl 6202 `"Actuarial Science"', add
label define degfield2d_lbl 6203 `"Business Management and Administration"', add
label define degfield2d_lbl 6204 `"Operations, Logistics and E-Commerce"', add
label define degfield2d_lbl 6205 `"Business Economics"', add
label define degfield2d_lbl 6206 `"Marketing and Marketing Research"', add
label define degfield2d_lbl 6207 `"Finance"', add
label define degfield2d_lbl 6209 `"Human Resources and Personnel Management"', add
label define degfield2d_lbl 6210 `"International Business"', add
label define degfield2d_lbl 6211 `"Hospitality Management"', add
label define degfield2d_lbl 6212 `"Management Information Systems and Statistics"', add
label define degfield2d_lbl 6299 `"Miscellaneous Business and Medical Administration"', add
label define degfield2d_lbl 6402 `"History"', add
label define degfield2d_lbl 6403 `"United States History"', add
label values degfield2d degfield2d_lbl

label define empstat_lbl 0 `"N/A"'
label define empstat_lbl 1 `"Employed"', add
label define empstat_lbl 2 `"Unemployed"', add
label define empstat_lbl 3 `"Not in labor force"', add
label values empstat empstat_lbl

label define empstatd_lbl 00 `"N/A"'
label define empstatd_lbl 10 `"At work"', add
label define empstatd_lbl 11 `"At work, public emerg"', add
label define empstatd_lbl 12 `"Has job, not working"', add
label define empstatd_lbl 13 `"Armed forces"', add
label define empstatd_lbl 14 `"Armed forces--at work"', add
label define empstatd_lbl 15 `"Armed forces--not at work but with job"', add
label define empstatd_lbl 20 `"Unemployed"', add
label define empstatd_lbl 21 `"Unemp, exper worker"', add
label define empstatd_lbl 22 `"Unemp, new worker"', add
label define empstatd_lbl 30 `"Not in Labor Force"', add
label define empstatd_lbl 31 `"NILF, housework"', add
label define empstatd_lbl 32 `"NILF, unable to work"', add
label define empstatd_lbl 33 `"NILF, school"', add
label define empstatd_lbl 34 `"NILF, other"', add
label values empstatd empstatd_lbl

label define classwkr_lbl 0 `"N/A"'
label define classwkr_lbl 1 `"Self-employed"', add
label define classwkr_lbl 2 `"Works for wages"', add
label values classwkr classwkr_lbl

label define classwkrd_lbl 00 `"N/A"'
label define classwkrd_lbl 10 `"Self-employed"', add
label define classwkrd_lbl 11 `"Employer"', add
label define classwkrd_lbl 12 `"Working on own account"', add
label define classwkrd_lbl 13 `"Self-employed, not incorporated"', add
label define classwkrd_lbl 14 `"Self-employed, incorporated"', add
label define classwkrd_lbl 20 `"Works for wages"', add
label define classwkrd_lbl 21 `"Works on salary (1920)"', add
label define classwkrd_lbl 22 `"Wage/salary, private"', add
label define classwkrd_lbl 23 `"Wage/salary at non-profit"', add
label define classwkrd_lbl 24 `"Wage/salary, government"', add
label define classwkrd_lbl 25 `"Federal govt employee"', add
label define classwkrd_lbl 26 `"Armed forces"', add
label define classwkrd_lbl 27 `"State govt employee"', add
label define classwkrd_lbl 28 `"Local govt employee"', add
label define classwkrd_lbl 29 `"Unpaid family worker"', add
label values classwkrd classwkrd_lbl

label define pwstate2_lbl 00 `"N/A"'
label define pwstate2_lbl 01 `"Alabama"', add
label define pwstate2_lbl 02 `"Alaska"', add
label define pwstate2_lbl 04 `"Arizona"', add
label define pwstate2_lbl 05 `"Arkansas"', add
label define pwstate2_lbl 06 `"California"', add
label define pwstate2_lbl 08 `"Colorado"', add
label define pwstate2_lbl 09 `"Connecticut"', add
label define pwstate2_lbl 10 `"Delaware"', add
label define pwstate2_lbl 11 `"District of Columbia"', add
label define pwstate2_lbl 12 `"Florida"', add
label define pwstate2_lbl 13 `"Georgia"', add
label define pwstate2_lbl 15 `"Hawaii"', add
label define pwstate2_lbl 16 `"Idaho"', add
label define pwstate2_lbl 17 `"Illinois"', add
label define pwstate2_lbl 18 `"Indiana"', add
label define pwstate2_lbl 19 `"Iowa"', add
label define pwstate2_lbl 20 `"Kansas"', add
label define pwstate2_lbl 21 `"Kentucky"', add
label define pwstate2_lbl 22 `"Louisiana"', add
label define pwstate2_lbl 23 `"Maine"', add
label define pwstate2_lbl 24 `"Maryland"', add
label define pwstate2_lbl 25 `"Massachusetts"', add
label define pwstate2_lbl 26 `"Michigan"', add
label define pwstate2_lbl 27 `"Minnesota"', add
label define pwstate2_lbl 28 `"Mississippi"', add
label define pwstate2_lbl 29 `"Missouri"', add
label define pwstate2_lbl 30 `"Montana"', add
label define pwstate2_lbl 31 `"Nebraska"', add
label define pwstate2_lbl 32 `"Nevada"', add
label define pwstate2_lbl 33 `"New Hampshire"', add
label define pwstate2_lbl 34 `"New Jersey"', add
label define pwstate2_lbl 35 `"New Mexico"', add
label define pwstate2_lbl 36 `"New York"', add
label define pwstate2_lbl 37 `"North Carolina"', add
label define pwstate2_lbl 38 `"North Dakota"', add
label define pwstate2_lbl 39 `"Ohio"', add
label define pwstate2_lbl 40 `"Oklahoma"', add
label define pwstate2_lbl 41 `"Oregon"', add
label define pwstate2_lbl 42 `"Pennsylvania"', add
label define pwstate2_lbl 44 `"Rhode Island"', add
label define pwstate2_lbl 45 `"South Carolina"', add
label define pwstate2_lbl 46 `"South Dakota"', add
label define pwstate2_lbl 47 `"Tennessee"', add
label define pwstate2_lbl 48 `"Texas"', add
label define pwstate2_lbl 49 `"Utah"', add
label define pwstate2_lbl 50 `"Vermont"', add
label define pwstate2_lbl 51 `"Virginia"', add
label define pwstate2_lbl 53 `"Washington"', add
label define pwstate2_lbl 54 `"West Virginia"', add
label define pwstate2_lbl 55 `"Wisconsin"', add
label define pwstate2_lbl 56 `"Wyoming"', add
label define pwstate2_lbl 61 `"Maine-New Hamp-Vermont"', add
label define pwstate2_lbl 62 `"Massachusetts-Rhode Island"', add
label define pwstate2_lbl 63 `"Minn-Iowa-Missouri-Kansas-S Dakota-N Dakota"', add
label define pwstate2_lbl 64 `"Mayrland-Delaware"', add
label define pwstate2_lbl 65 `"Montana-Idaho-Wyoming"', add
label define pwstate2_lbl 66 `"Utah-Nevada"', add
label define pwstate2_lbl 67 `"Arizona-New Mexico"', add
label define pwstate2_lbl 68 `"Alaska-Hawaii"', add
label define pwstate2_lbl 72 `"Puerto Rico"', add
label define pwstate2_lbl 73 `"U.S. outlying area"', add
label define pwstate2_lbl 74 `"United States (1980 Puerto Rico samples)"', add
label define pwstate2_lbl 80 `"Abroad"', add
label define pwstate2_lbl 81 `"Europe"', add
label define pwstate2_lbl 82 `"Eastern Asia"', add
label define pwstate2_lbl 83 `"South Central, South East, and Western Asia"', add
label define pwstate2_lbl 84 `"Mexico"', add
label define pwstate2_lbl 85 `"Other Americas"', add
label define pwstate2_lbl 86 `"Other, nec"', add
label define pwstate2_lbl 87 `"Iraq"', add
label define pwstate2_lbl 88 `"Canada"', add
label define pwstate2_lbl 90 `"Confidential"', add
label define pwstate2_lbl 99 `"Not reported"', add
label values pwstate2 pwstate2_lbl

