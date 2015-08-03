'Copyright (c) 2015 Ralt Software

'This program is free software: you can redistribute it and/or modify
'it under the terms of the GNU General Public License version 3, as
'published by the Free Software Foundation.

'This program is distributed in the hope that it will be useful,
'but WITHOUT ANY WARRANTY; without even the implied warranty of
'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'GNU General Public License for more details.

'You should have received a copy of the GNU General Public License
'along with this program.  If not, see <http://www.gnu.org/licenses/>.

TYPE Item
    Name AS STRING * 20
    Count AS INTEGER
END TYPE

TYPE NPC
    Name AS STRING * 15
    Race AS STRING * 15
    RaceName AS STRING * 15
    Class AS STRING * 15
    ClassName AS STRING * 15
    Level AS INTEGER
    Health AS INTEGER
    Speed AS INTEGER
    WaterSpeed AS INTEGER
    ViewDistance AS INTEGER
    NightView AS INTEGER
    Attack AS INTEGER
    Defence AS INTEGER
    Dexterity AS INTEGER
    RegenerationSpeed AS INTEGER
    Flag AS INTEGER
    Loot AS Item
END TYPE

COMMON SHARED MAPX%
COMMON SHARED MAPY%
COMMON SHARED HH%
COMMON SHARED L$
COMMON SHARED PLAYER_CLASS$
COMMON SHARED SCO$
COMMON SHARED W$
COMMON SHARED WW%
COMMON SHARED STROKA$
COMMON SHARED Item$
DIM SHARED Player AS NPC

DIM SHARED Inventory(20) AS Item

'Keys'
LF$ = CHR$(0) + CHR$(75): RT$ = CHR$(0) + CHR$(77): UP$ = CHR$(0) + CHR$(72): EN$ = CHR$(0) + CHR$(13): DN$ = CHR$(0) + CHR$(80): Esc$ = CHR$(27)

'Title
_TITLE "Symbol World v0.0.9a"

'ICON'
i& = _LOADIMAGE("SYMBOL.png", 32)

IF i& < -1 THEN
    _ICON i&
    _FREEIMAGE i&
END IF

'Config load process'
CONFIG$ = "config.ini"
'OPEN CONFIG$ FOR INPUT AS #3

'Graphic modes load'
Graphics:

SCREEN _NEWIMAGE(640, 480, 12)
_SCREENMOVE _MIDDLE
_MOUSEHIDE

'Intro'
Intro:

'Sound / Music loading'

wav1& = _SNDOPEN("Data/Sounds/okey1.wav", "sync,vol")
_SNDVOL wav1&, 0.7

mp31& = _SNDOPEN("Data/Music/Menu/Intro.mp3", "vol")
_SNDVOL mp31&, 0.7

IF NOT _SNDPLAYING(mp31&) THEN
    _SNDPLAY mp31&
END IF

CALL ShowIntro

IF NOT _SNDPLAYING(wav1&) THEN
    _SNDPLAY wav1&
END IF

_SNDSTOP wav&

Menu: 'Menu with pars'

DIM Entries$(4)
Entries$(0) = "Start Game"
Entries$(1) = "Load Game"
Entries$(2) = "Options"
Entries$(3) = "About"
Entries$(4) = "Exit"

DO
    SELECT CASE ShowMenu%("MENU", 4, Entries$())
        CASE 0: IF NOT _SNDPLAYING(wav1&) THEN _SNDPLAY wav1&: _SNDSTOP wav&: GOSUB Mods
        CASE 1: IF NOT _SNDPLAYING(wav1&) THEN _SNDPLAY wav1&: _SNDSTOP wav&: GOTO Load
        CASE 2: IF NOT _SNDPLAYING(wav1&) THEN _SNDPLAY wav1&: _SNDSTOP wav&: GOSUB Mods
        CASE 3: IF NOT _SNDPLAYING(wav1&) THEN _SNDPLAY wav1&: _SNDSTOP wav&: COLOR 15, 0: CLS: DisplayFile "Data/About.txt", 1, 30
        CASE 4: IF NOT _SNDPLAYING(wav1&) THEN _SNDSTOP wav&: END
        CASE -1: IF NOT _SNDPLAYING(wav1&) THEN _SNDSTOP wav&: END
    END SELECT
LOOP

Mods:
DIM ModsEntries$(2)

DO
    ModsEntries$(0) = "Adventure Mode"
    ModsEntries$(1) = "Tower Defence Mode"
    ModsEntries$(2) = "Scenario Mode"

    SELECT CASE ShowMenu%("SELECT MODE", 2, ModsEntries$())
        CASE 0: IF NOT _SNDPLAYING(wav1&) THEN _SNDPLAY wav1&: _SNDSTOP wav&: M% = 1: GOSUB Profile
        CASE 1: IF NOT _SNDPLAYING(wav1&) THEN _SNDPLAY wav1&: _SNDSTOP wav&: M% = 2: GOSUB Profile
        CASE 2: IF NOT _SNDPLAYING(wav1&) THEN _SNDPLAY wav1&: _SNDSTOP wav&: M% = 3: GOSUB Profile
    END SELECT
LOOP
RETURN

Profile:

DIM ProfileEntries$(3)

DO
    ProfileEntries$(0) = "Enter your name:" + Player.Name
    ProfileEntries$(1) = "Choose your class:" + Player.Class
    ProfileEntries$(2) = "Choose your race:" + Player.Race
    ProfileEntries$(3) = "Accept"

    SELECT CASE ShowMenu%("PROFILE OPTIONS", 3, ProfileEntries$())
        CASE 0: IF NOT _SNDPLAYING(wav1&) THEN _SNDPLAY wav1&: _SNDSTOP wav&: LOCATE 4, 23: INPUT "", Player.Name
        CASE 1: IF NOT _SNDPLAYING(wav1&) THEN _SNDPLAY wav1&: _SNDSTOP wav&: CLS: GOSUB Class
        CASE 2: IF NOT _SNDPLAYING(wav1&) THEN _SNDPLAY wav1&: _SNDSTOP wav&: CLS: GOSUB Race
        CASE 3: IF NOT _SNDPLAYING(wav1&) THEN _SNDPLAY wav1&: _SNDSTOP wav&: CLS: CalculateAttributes Player: GOSUB Scenario: GOTO Loading_Map
        CASE -1: EXIT DO
    END SELECT
LOOP
RETURN

Scenario:
L$ = "TOWNS" 'Map Level'
N$ = "DEMO" 'Map name

'MSC$ = "Data/Maps/MAP_" + N$ + "/Scenario/Music.txt"

SCREEN _NEWIMAGE(640, 480, 12)

DisplayFile "Data/Maps/MAP_" + L$ + "/Scenario/" + RTRIM$(Player.Class) + ".txt", 1, 30

RETURN

Loading_Map: 'Loading Map process'
CLS

MAPFILE$ = "Data/Maps/MAP_TOWNS/MAP_" + RTRIM$(UCASE$(Player.Race)) + "_TOWN.msbw"

CLS

AccessNum% = FREEFILE
OPEN MAPFILE$ FOR INPUT AS AccessNum%
LINE INPUT #AccessNum%, W$: WW% = VAL(W$) 'Get map width
LINE INPUT #AccessNum%, H$: HH% = VAL(H$) 'Get map height

DIM SHARED MAP$(HH%)

FOR I = 0 TO HH% - 1
    LINE INPUT #AccessNum%, MAP$(I) 'Write in array data'
    IF X% = 0 THEN
        X% = INSTR(MAP$(I), "P")
        Y% = I
    END IF
NEXT I
CLOSE #1

_SNDSTOP mp31&
_SNDCLOSE mp31&

mp32& = _SNDOPEN("Data/Music/Towns/" + Player.Race + ".mp3", "vol")
_SNDVOL mp32&, 0.7

IF NOT _SNDPLAYING(mp32&) THEN
    _SNDPLAY mp32&
END IF

Game:
Player.Health = 100 'Player HP
Player.Level = 1 'Player level

COLOR 15, 0

DO
    CLS

    COLOR 7: CALL RenderMap

    COLOR 15

    PRINT "##################################INFORMATION###################################"
    PRINT "#    HP:            Race:                                       Time:  0:00    #"
    PRINT "#    MP:            Class:                                                     #"
    PRINT "################################################################################"

    _PRINTSTRING (60, 15), STR$(Player.Health)
    _PRINTSTRING (370, 15), "Gold: " + STR$(Player.Level)
    _PRINTSTRING (230, 15), Player.Race
    _PRINTSTRING (60, 30), STR$(MP%)
    _PRINTSTRING (370, 30), "Name: " + Player.Name
    _PRINTSTRING (230, 30), Player.Class

    _PRINTSTRING (0, 360), "#####################################LOG########################################"
    _PRINTSTRING (0, 375), "#                                                                              #"
    _PRINTSTRING (0, 390), "#                                                                              #"
    _PRINTSTRING (0, 405), "#                                                                              #"
    _PRINTSTRING (0, 420), "#                                                                              #"
    _PRINTSTRING (0, 435), "################################################################################"
    _PRINTSTRING (0, 450), "#  Enter:                                                                      #"
    _PRINTSTRING (0, 465), "################################################################################"

    _DISPLAY

    a$ = INKEY$

    IF a$ <> "" THEN

        IF NOT ASC(MAP$(Y%), X%) = 126 THEN
            ASC(MAP$(Y%), X%) = 45
        END IF

        IF NOT ASC(MAP$(Y%), X%) = 45 THEN
            ASC(MAP$(Y%), X%) = 126
        END IF

        SELECT CASE a$
            CASE CHR$(0) + "H": 'up
                OLDY% = Y%: Y% = Y% - 1:
                OLDX% = X%
                MAPY% = MAPY% + 1
            CASE CHR$(0) + "P": 'down
                OLDY% = Y%: Y% = Y% + 1
                OLDX% = X%
                MAPY% = MAPY% - 1
            CASE CHR$(0) + "K": 'left
                OLDX% = X%: X% = X% - 1
                OLDY% = Y%
                MAPX% = MAPX% + 1
            CASE CHR$(0) + "M": 'right
                OLDX% = X%: X% = X% + 1
                OLDY% = Y%
                MAPX% = MAPX% - 1
            CASE CHR$(73): DisplayInventory
            CASE CHR$(13):
                LOCATE 29, 10: INPUT "", COM$
            CASE CHR$(27): END
        END SELECT

        SELECT CASE ASC(MAP$(Y%), X%)
            CASE ASC("#"): X% = OLDX%: Y% = OLDY%
            CASE ASC("*"):
                DIM GoldItem AS Item
                GoldItem.Name = "Gold"
                GoldItem.Count = 1
                GiveItem GoldItem: GOLD% = GOLD% + 1
            CASE ASC("N"):
                X% = OLDX%: Y% = OLDY%
            CASE ASC("G"):
                X% = OLDX%: Y% = OLDY%
        END SELECT

        ASC(MAP$(Y%), X%) = Player.Flag

    END IF
LOOP

Errors:
'Ydalil, a to naxui oni komy y sdalis.
'Lucshe suka yvidet ekran smerti,  chem "ARRAY OF OUT!"
'Da.

Load:

Options:

Class:

DIM ClassEntries$(3)
ClassEntries$(0) = "Warrior"
ClassEntries$(1) = "Archer"
ClassEntries$(2) = "Mage"
ClassEntries$(3) = "Thief"

Player.Class = ClassEntries$(ShowMenu%("CHOOSE YOUR CLASS", 3, ClassEntries$()))
_SNDPLAY wav1&: _SNDSTOP wav&
RETURN

Race:

DIM RaceEntries$(6)
RaceEntries$(0) = "Human"
RaceEntries$(1) = "Cat Man"
RaceEntries$(2) = "Dog Man"
RaceEntries$(3) = "Fish Man"
RaceEntries$(4) = "Frog Man"
RaceEntries$(5) = "Lizard Man"
RaceEntries$(6) = "Skeleton"

Player.Race = RaceEntries$(ShowMenu%("CHOOSE YOUR RACE", 6, RaceEntries$()))
_SNDPLAY wav1&: _SNDSTOP wav&
RETURN

FUNCTION ShowMenu% (Header$, NumEntries%, Entries$())
PAR% = 0
DO
    WI% = 13
    HI% = 13

    COLOR 15, 1
    CLS

    COLOR 15: PRINT "################################################################################"

    FOR I = 0 TO 25
        COLOR 15: PRINT "#                                                                              #"
    NEXT

    COLOR 15: PRINT "################################################################################"

    COLOR 15: _PRINTSTRING (300, 30), Header$

    FOR I = 0 TO NumEntries%
        BG_COLOR% = 6
        IF I = PAR% THEN BG_COLOR% = 4
        COLOR 15, BG_COLOR%
        _PRINTSTRING (50, 50 + 18 * I), Entries$(I)
    NEXT I

    _DISPLAY

    SELECT CASE INKEY$
        CASE CHR$(0) + "H":
            PAR% = PAR% - 1
            IF PAR% < 0 THEN PAR% = NumEntries%
        CASE CHR$(0) + "P":
            PAR% = PAR% + 1
            IF PAR% > NumEntries% THEN PAR% = 0
        CASE CHR$(13)
            EXIT DO
        CASE CHR$(27)
            PAR% = -1
            EXIT DO
    END SELECT
LOOP
ShowMenu% = PAR%
END FUNCTION

SUB RenderMap
FOR I = 0 TO HH% - 1
    _PRINTSTRING (MAPX% * _FONTWIDTH, (MAPY% + I) * _FONTHEIGHT), MAP$(I)
NEXT I
END SUB

SUB GiveItem (Target AS Item)
FOR I = 1 TO 20
    IF RTRIM$(Inventory(I).Name) = "" OR RTRIM$(Inventory(I).Name) = Target.Name THEN Inventory(I).Name = Target.Name: Inventory(I).Count = Inventory(I).Count + Target.Count: EXIT FOR
NEXT I
END SUB

SUB DisplayInventory
COLOR 15, 0
CLS
FOR I = 1 TO 20
    PRINT RTRIM$(Inventory(I).Name + " X" + STR$(Inventory(I).Count))
NEXT I

_DISPLAY

DO WHILE NOT INKEY$ = CHR$(27)
LOOP

END SUB

SUB HashMapSet (Map$( ,), Key$, Value$)

DO WHILE NOT (Map$(I%, 0) = "" OR Map$(I%, 0) = Key$)
    I% = I% + 1
LOOP

Map$(I%, 0) = Key$
Map$(I%, 1) = Value$

END SUB

FUNCTION HashMapGet$ (Map$( ,), Key$)

DO WHILE NOT Map$(I%, 0) = Key$
    I% = I% + 1
LOOP

HashMapGet$ = Map$(I%, 1)

END SUB

SUB ReadConfig (HashMap$( ,), Config$)

AccessNum% = FREEFILE
OPEN Config$ FOR INPUT AS AccessNum%

DO WHILE NOT EOF(AccessNum%)

    LINE INPUT #AccessNum%, KeyValuePair$

    IF KeyValuePair$ <> "" THEN

        Key$ = LEFT$(KeyValuePair$, INSTR(KeyValuePair$, "=") - 1)
        Value$ = RIGHT$(KeyValuePair$, LEN(KeyValuePair$) - LEN(Key$) - 1)

        HashMapSet HashMap$(), RTRIM$(LTRIM$(Key$)), RTRIM$(LTRIM$(Value$))

    END IF

LOOP

CLOSE AccessNum%

END SUB

SUB CalculateAttributes (Target AS NPC)

DIM RaceAttributes$(9, 1)
DIM ClassAttributes$(4, 1)

ReadConfig RaceAttributes$(), "Data/Races/" + RTRIM$(Target.Race) + ".race"
ReadConfig ClassAttributes$(), "Data/Classes/" + RTRIM$(Target.Class) + ".class"
Target.RaceName = HashMapGet$(RaceAttributes$(), "RACE_NAME")
Target.ClassName = HashMapGet$(ClassAttributes$(), "CLASS_NAME")
Target.Speed = VAL(HashMapGet$(RaceAttributes$(), "RACE_SPEED"))
Target.WaterSpeed = VAL(HashMapGet$(RaceAttributes$(), "RACE_SWATER"))
Target.ViewDistance = VAL(HashMapGet$(RaceAttributes$(), "RACE_VISIBILITY"))
Target.NightView = VAL(HashMapGet$(RaceAttributes$(), "RACE_VNIGHT"))
Target.Attack = VAL(HashMapGet$(RaceAttributes$(), "RACE_ATTACK")) + VAL(HashMapGet$(ClassAttributes$(), "CLASS_ATTACK"))
Target.Defence = VAL(HashMapGet$(RaceAttributes$(), "RACE_DEFENCE")) + VAL(HashMapGet$(ClassAttributes$(), "CLASS_DEFENCE"))
Target.Dexterity = VAL(HashMapGet$(ClassAttributes$(), "CLASS_DEXTERITY"))
Target.RegenerationSpeed = VAL(HashMapGet$(RaceAttributes$(), "RACE_REGENERATION"))
Target.Flag = ASC(HashMapGet$(RaceAttributes$(), "RACE_FLAG"))

COLOR 15, 0: CLS
PRINT "NAME:" + Target.Name
PRINT "RACE:" + Target.RaceName
PRINT "CLASS:" + Target.ClassName
PRINT "SPEED:" + STR$(Target.Speed)
PRINT "SPEED IN WATER:" + STR$(Target.WaterSpeed)
PRINT "VISIBILITY:" + STR$(Target.ViewDistance)
PRINT "NIGHT VISIBILITY:" + STR$(Target.NightView)
PRINT "ATTACK:" + STR$(Target.Attack)
PRINT "DEFENCE:" + STR$(Target.Defence)
PRINT "DEXTERITY:" + STR$(Target.Dexterity)
PRINT "REGENERATION:" + STR$(Target.RegenerationSpeed)
PRINT "PLAYER FLAG:" + STR$(Target.Flag)
_DISPLAY
_DELAY 5

END SUB

SUB DisplayDialog (Text$, Name$)

Dialog$ = "Data/Dialog_Window.txt"

IF _FILEEXISTS(Dialog$) THEN
    OPEN Dialog$ FOR INPUT AS #1

    IF Name$ = "" THEN
    END IF

    REDIM SC$(0)
    DO WHILE NOT EOF(1)
        REDIM _PRESERVE SC$(I)
        LINE INPUT #1, SC$(I)
        I = I + 1
    LOOP

    CLOSE #1

    IF (I - 1) * _FONTHEIGHT < _HEIGHT THEN SCY% = _HEIGHT \ 2 - (I - 1) * _FONTHEIGHT \ 2

    DO
        FOR J = 0 TO I - 1
            COLOR 14: _PRINTSTRING (_WIDTH \ 2 - _PRINTWIDTH(SC$(J)) \ 2, SCY% + _FONTHEIGHT * J), SC$(J)
        NEXT J

        COLOR 14: _PRINTSTRING (278, 80), Name$
        COLOR 6: _PRINTSTRING (8, 108), Text$

        _DISPLAY
        _DELAY 0.2
        IF SCY% <= -(_FONTHEIGHT * I) OR INKEY$ = CHR$(27) THEN EXIT DO
    LOOP
END IF

CLOSE #1

END SUB


SUB DisplayFile (Path$, Mode%, Speed%)
IF _FILEEXISTS(Path$) THEN
    OPEN Path$ FOR INPUT AS #1

    IF Mode% = 0 THEN
    END IF

    REDIM SC$(0)
    DO WHILE NOT EOF(1)
        REDIM _PRESERVE SC$(I)
        LINE INPUT #1, SC$(I)
        I = I + 1
    LOOP

    CLOSE #1

    IF (I - 1) * _FONTHEIGHT < _HEIGHT THEN SCY% = _HEIGHT \ 2 - (I - 1) * _FONTHEIGHT \ 2

    DO
        FOR J = 0 TO I - 1
            _PRINTSTRING (_WIDTH \ 2 - _PRINTWIDTH(SC$(J)) \ 2, SCY% + _FONTHEIGHT * J), SC$(J)
        NEXT J
        _DISPLAY
        _LIMIT Speed%

        IF Mode% = 1 THEN
            SCY% = SCY% - 1
        END IF

        IF SCY% <= -(_FONTHEIGHT * I) OR INKEY$ = CHR$(27) THEN EXIT DO
    LOOP
END IF

CLOSE #1

END SUB

SUB NPC.Load
IF _DIREXISTS("Data/MAP_TOWNS/NPC") THEN
    IF _FILEEXISTS("Data/MAP_TOWNS/NPC/NPC_LIST.txt") THEN

    END IF
END IF

END SUB

SUB ShowIntro

DO WHILE INKEY$ = ""

    CLS

    COLOR 15, 1

    COLOR 14, 1: _PRINTSTRING (250, 100), "SYMBOL WORLD!"

    _LIMIT 60
    COLOR 8: _PRINTSTRING (200, 400), "PRESS ANY KEY TO CONTINUE"
    COLOR 6: _PRINTSTRING (275, 112), "0.0.9a"

    _LIMIT 60
    COLOR 8: _PRINTSTRING (200, 400), "PRESS ANY KEY TO CONTINUE"

LOOP

END SUB
