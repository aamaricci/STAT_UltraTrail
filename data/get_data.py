import pandas as pd
import numpy as np
import subprocess
import sys, os, shutil, csv

# all   = all
# 2023  -> 1960
YEAR=2023



# all   = all
# 50km  = 50km
# 50mi  = 50mi
# 100km = 100km
# 100mi = 100mi
# 1     = 45-79km
# 2     = 80-119km
# 4     = 120-179km
# 8     = 180+km
# 6h    = 6h
# 12h   = 12h
# 24h   = 24h
# 48h   = 48h
# 72h   = 72h
# 6d    = 6days
# 10d   = 10days
DIST=1
match DIST:
    case '1':
        LENGTH="45-79Km"
    case '2':
        LENGTH="80-119Km"
    case '4':
        LENGTH="120-179Km"
    case '8':
        LENGTH="180+ Km"
    case other:
        LENGTH=DIST



# all = World
# 1   = Europe
# 2   = Asia
# 3   = Africa
# 4   = North America
# 5   = South America
# 6   = Oceania
# ALB = ALB - Albania
# ALG = ALG - Algeria
# AND = AND - Andorra
# ARG = ARG - Argentina
# ARU = ARU - Aruba
# ATA = ATA - Antarctic
# AUS = AUS - Australia
# AUT = AUT - Austria
# BEL = BEL - Belgium
# BER = BER - Bermuda
# BHU = BHU - Bhutan
# BIH = BIH - Bosnia and Herzegovina
# BLR = BLR - Belarus
# BOL = BOL - Bolivia
# BOT = BOT - Botswana
# BRA = BRA - Brazil
# BRU = BRU - Brunei
# BUL = BUL - Bulgaria
# BUR = BUR - Burkina Faso
# CAM = CAM - Cambodia
# CAN = CAN - Canada
# CAY = CAY - Cayman Islands
# CHA = CHA - Chad
# CHI = CHI - Chile
# CHN = CHN - China
# CMR = CMR - Cameroon
# COL = COL - Colombia
# CPV = CPV - Cape Verde
# CRC = CRC - Costa Rica
# CRO = CRO - Croatia
# CUB = CUB - Cuba
# CYP = CYP - Cyprus
# CZE = CZE - Czech Republic
# DEN = DEN - Denmark
# DJI = DJI - Djibouti
# DOM = DOM - Dominican Republic
# ECU = ECU - Ecuador
# EGY = EGY - Egypt
# ESA = ESA - El Salvador
# ESP = ESP - Spain
# EST = EST - Estonia
# FIN = FIN - Finland
# FRA = FRA - France
# GBR = GBR - United Kingdom
# GEO = GEO - Georgia
# GER = GER - Germany
# GRE = GRE - Greece
# GRL = GRL - Greenland
# GUA = GUA - Guatemala
# GUM = GUM - Guam
# HKG = HKG - Hong Kong, China
# HUN = HUN - Hungary
# INA = INA - Indonesia
# IND = IND - India
# IRI = IRI - Iran
# IRL = IRL - Ireland
# ISL = ISL - Iceland
# ISR = ISR - Israel
# ITA = ITA - Italy
# IVB = IVB - British Virgin Islands
# JOR = JOR - Jordan
# JPN = JPN - Japan
# KAZ = KAZ - Kazakhstan
# KEN = KEN - Kenya
# KGZ = KGZ - Kyrgyzstan
# KOR = KOR - South Korea
# KOS = KOS - Kosovo
# KSA = KSA - Saudi Arabia
# LAO = LAO - Laos
# LAT = LAT - Latvia
# LBA = LBA - Libya
# LBN = LBN - Lebanon
# LES = LES - Lesotho
# LIE = LIE - Liechtenstein
# LTU = LTU - Lithuania
# LUX = LUX - Luxembourg
# MAC = MAC - Macao, China
# MAD = MAD - Madagascar
# MAR = MAR - Morocco
# MAS = MAS - Malaysia
# MDA = MDA - Moldova
# MEX = MEX - Mexico
# MGL = MGL - Mongolia
# MKD = MKD - North Macedonia
# MLI = MLI - Mali
# MLT = MLT - Malta
# MNE = MNE - Montenegro
# MON = MON - Monaco
# MOZ = MOZ - Mozambique
# MRI = MRI - Mauritius
# MTN = MTN - Mauritania
# MYA = MYA - Myanmar
# NAM = NAM - Namibia
# NCA = NCA - Nicaragua
# NED = NED - Netherlands
# NEP = NEP - Nepal
# NOR = NOR - Norway
# NZL = NZL - New Zealand
# OMA = OMA - Oman
# PAK = PAK - Pakistan
# PAN = PAN - Panama
# PAR = PAR - Paraguay
# PER = PER - Peru
# PHI = PHI - Philippines
# POL = POL - Poland
# POR = POR - Portugal
# PUR = PUR - Puerto Rico
# QAT = QAT - Qatar
# ROU = ROU - Romania
# RSA = RSA - South Africa
# RUS = RUS - Russia
# SEN = SEN - Senegal
# SEY = SEY - Seychelles
# SGP = SGP - Singapore
# SLO = SLO - Slovenia
# SRB = SRB - Serbia
# SRI = SRI - Sri Lanka
# STP = STP - São Tomé and Príncipe
# SUI = SUI - Switzerland
# SVK = SVK - Slovakia
# SWE = SWE - Sweden
# TAN = TAN - Tanzania
# THA = THA - Thailand
# TPE = TPE - Taiwan
# TRI = TRI - Trinidad and Tobago
# TUN = TUN - Tunisia
# TUR = TUR - Turkey
# UAE = UAE - United Arab Emirates
# UKR = UKR - Ukraine
# URU = URU - Uruguay
# USA = USA - United States
# VEN = VEN - Venezuela
# VIE = VIE - Vietnam
# ZIM = ZIM - Zimbabwe
COUNTRY='ITA'



# all   = all
# Road  = road race
# Trail = Trail
# Stage = stage race
# Track = track
# Indoo = indoor
# Elim  = elimination race
# Backy = Backyard Ultra
# Walk  = Ultra-walking
SURFACE='Trail'



URL_DB='https://statistik.d-u-v.org/geteventlist.php?year='+str(YEAR)+'&dist='+str(DIST)+'&country='+str(COUNTRY)+'&Submit.x=16&Submit.y=10&label=&surface='+str(SURFACE)+'&sort=1&from=&to='
print("Retrieving data from"+str(URL_DB))
print("YEAR="+str(YEAR))
print("DIST="+str(LENGTH))
print("COUNTRY="+str(COUNTRY))
print("SURFACE="+str(SURFACE))

db=pd.read_html(URL_DB,extract_links="all")

file="list.race"
if os.path.isfile(file):
    os.remove(file)
flist = open(file, "a")

links=[]
for i in range(len(db[-1])):
    name=str(db[-1].values[i][1][1])
    if name!='None':
        links.append(name.split("=")[-1])

cols_list=["Rank", "Performance", "Name", "Club", "Nat.", "Birth Year", "M/F", "Rank M/F", "Cat", "Cat. Rank", "Avg.Speed km/", "Adj. Performance"]

new_cols_list=["Rank", "Performance", "Nat.", "Birth Year", "M/F", "Rank M/F", "Cat", "Cat. Rank", "Avg.Speed km/"]
for link in links[:1]:
    url = 'https://statistik.d-u-v.org/getresultevent.php?event='+str(link)
    table=pd.read_html(url)
    df = table[-1]
    
    #NAME OF THE EVENT:
    chars_1=[" ","'", "^"]
    chars_2=[';', ':', '!', "*","(",")","°"]
    NAME=str(table[-3].values[1][1])
    for i in chars_1:
        NAME = NAME.replace(i,"_")
    for i in chars_2:
        NAME = NAME.replace(i,"")
    flist.write(NAME+"\n")
    print(NAME)
    
    t = open("try.csv", "w")
    df.to_csv("tmp.csv", header=False, index=False, sep=';')
    subprocess.call(["sed", 's/ h//g', "tmp.csv"], stdout=t)
    if os.path.isfile('tmp.csv'):
        os.remove('tmp.csv')
    df = pd.read_csv("try.csv",names=cols_list,sep=';')
    if os.path.isfile('try.csv'):
        os.remove('try.csv')
    t.close()    
    df.to_csv("input_"+str(NAME)+".csv", header=False, index=False, sep=' ', columns=new_cols_list)
        
