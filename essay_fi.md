% TIEA241 Automaatit ja kieliopit: tagenerator, tekstiseikkailupeligeneraattori
% Jaakko Pallari
% 15.8.2013

Tässä kirjoitelmassa tarkastellaan [tagenerator][]-ohjelman toimintaa.
Tagenerator on tietokoneohjelma, jolla voidaan generoida pelattavia
tekstiseikkailupelejä ohjelmalle suunnitellusta kontekstittomasta kielestä.
Tässä kirjoitelmassa erityisesti keskitytään tarkastelemaan ohjelmassa
hyödynnettäviä automaatteja ja kielioppeja.

Ensimmäisessä luvussa kerrotaan kuinka ohjelma käännetään ja kuinka sitä
käytetään. Toinen luku käsittelee tekstiseikkailupelien kuvaamiseen käytettyä
kieltä, ja kolmas luku käsittelee vuorostaan pelaajan komentoja tulkkaavaa
kieltä. Neljännessä luvussa pohditaan generoidun tekstiseikkalupelin rakennetta
automaattina. Viimeisessä luvussa tarkastellaan muita harjoitustyöhön liittyviä
kokemuksia ja haasteita.

# Kääntäminen ja käyttäminen

Tageneratorin lähdekoodit ovat saatavilla GitHub-sivustolta:

    https://github.com/jkpl/tagenerator

Ohjelma on toteutettu [Haskell][]-ohjelmointikielellä, joten ohjelman
kääntämiseen tarvitaan jonkinlainen Haskell-kääntäjä. Alla olevasta osoitteesta
voi ladata sopivat työkalut.

    http://www.haskell.org/platform/

Samat työkalut löytyvät tyypillisesti useiden Linux-jakeluiden
pakettihallintasovellusten avulla nimellä `haskell-platform`.

Ohjelman kääntäminen onnistuu Haskell Platformista saatavalla
`cabal`-komennolla. Mene komentokehotteessa tageneratorin lähdekoodihakemistoon
ja aja seuraavat komennot:

    $ cabal configure
    $ cabal build

Nyt lähdekoodihakemistosta pitäisi löytyä hakemisto `dist/build/tagenerator/`,
josta löytyy ajettava binääritiedosto `tagenerator`.

Ohjelma ottaa ensimmäiseksi parametrikseen polun hakemistoon, josta
tekstiseikkalupelin tiedostot löytyvät:

    $ dist/build/tagenerator/tagenerator examplegame/

Ohjelma yrittää löytää tekstiseikkailupelin tiedostot nykyisestä hakemistosta
mikäli polkua ei anneta parametriksi. Jokaisen tekstiseikkailupelin tiedoston
tulee loppua tiedostopäätteeseen `ta` (esimerkiksi: `esimerkki_peli.ta`).
Yksittäinen tekstiseikkailupeli voidaan (mutta ei ole pakko) jakaa useampaan
tiedostoon.

# Pelin rakennetta kuvaava kieli

Tekstiseikkailupelejä voidaan muodostaa vapaamuotoisella kontekstittomalla
kielellä, jonka osia voidaan kuvailla seuraavanlaisilla ABNF-säännöillä:

    document = *assignment
    assignment = identifier "=" item ";"
    identifier = *(ALPHA / DIGIT)
    item = block / typed_block / list
         / string_literal / variable
    block = "{" *assignment "}"
    typed_block = *(ALPHA / DIGIT) block
    list = [ *item ]
    string_literal = "\"" *VCHAR "\""
    variable = *(ALPHA / DIGIT)

Esimerkki kielen mukaisesta dokumentista:

    example1 = mytype {
        foo = "bar";
        somelist = [
            { first = example2; second = "xxx"; }
            "hello world"
        ];
    };
    example2 = [ ];

Jokaisesta tekstiseikkailusta on kuitenkin löydyttävä seuraavat ominaisuudet.
Dokumentin juuritasolla on sijoitettava aloitushuone muuttujaan `start_room`.
Jokainen huone on `typed_block` nimellä `room`, ja huoneet sisältävät seuraavat
ominaisuudet:

* `name` on nimi, ja tyypiltään `string_literal`
* `description` on huoneen kuvaus, ja tyypiltään `string_literal`
* `directions` on kokoelma suuntia (tyypiltään `block`), joista pääsee muihin
  huoneisiin. Jokainen suunta on sijoitus, jossa vasemmanpuoleinen operandi
  kertoo suunnan ja oikeanpuoleinen on viite (tyypiltään siis `variable`)
  toiseen huoneeseen dokumentissa.
* `items` on lista (`list`) huoneen sisältämiä esineitä.

Jokainen esine on `typed_block` nimellä `item`, ja esineet sisältävät
seuraavat ominaisuudet:

* `name` ja `description` vastaavasti kuin huoneilla.
* `actions` on lista tapahtumista joita esineeseen voidaan kohdistaa.

Tapahtumat ovat `typed_block` tyyppisiä, ja niiden nimet voivat olla joko
`pickup` (esineen nostamiseen) tai `look` (esineiden katselemiseen).
Tapahtumien ominaisuudet:

* `success` on tyypiltään `variable`, ja on oltava arvoltaan joko "yes" tai
  "no". Arvo kertoo onnistuuko tapahtuma vai ei.
* `message` on tyypiltään `string_literal`, ja kertoo tapahtumasta seuraavan
  tekstin.

Ohjelmassa tekstiseikkailut jäsennetään kahdessa vaiheessa. Ensimmäiseksi
tekstidokumenteista muodostetaan abstrakti syntaksipuu, joka saattaa sisältää
tekstiseikkailulle oleellisesta datasta. Toisessa vaiheessa kootusta puusta
poimitaan tekstiseikkailulle oleellinen data, ja muodostetaan niistä pelin
käyttöön sopivat tietorakenteet.

Ensimmäisessä vaiheessa jäsentäminen tapahtuu yhdistämällä eri tavoin useita
yksinkertaisempia jäsentimiä, joista yksinkertaisimmat jäsentimet lukevat
tekstidokumentista yhden merkin kerrallaan. Jos yksittäinen epäonnistuu
jäsentämisessä, sen vaikutukset kaskadoituvat ylöspäin kunnes jokin ylemmistä
jäsentimistä käsittelee ongelman tai jäsennys epäonnistuu kokonaan. Alemman
jäsentimen onnistuessa, jäsentimen tulos otetaan tai ollaan ottamatta talteen
riippuen ylemmästä jäsentimestä ja jatketaan jäsentämistä ylemmän jäsentimen
viittomalla tavalla. Kun ylin jäsennin on saatu päätökseen, tuloksena saadaan
ylimmän jäsentimen tulos. Jokainen ohjelman jäsennin tekee päättelynsä vain ja
ainoastaan sillä hetkellä saatavilla olevan jäsennettävän datan perusteella.

Jäsentimet toteuttavat monadien, alternativen ja aplikatiivisten funktoreiden
rajapinnat. Siispä jäsentimien yhdisteleminen onnistuu käyttämällä kyseisten
tyyppiluokkien operaatioita. Näin jäsentimiä toteuttaessa oli mahdollista
hyödyntää lukuisia korkean tason funktioita, jotka käyttävät kyseisiä
operaatioita.

Kieli sisältää rekursiivisia tietorakenteita. Jäsennettävä data saattaa
sisältää esimerkiksi `block` -tyyppistä, joka voi koostua edelleen uusista
rekursiivisista tietorakenteista. Eräs kielen tietotyypeistä toimii tapana
viitata toisiin tietorakenteisiin, joka voisi mahdollistaa muodostaa graafia
muodostavan tietorakenteen. Viittauksia ei kuitenkaan ratkota jäsennyksen
aikana, jolloin jäsennyksen tuloksena saatu objekti on aina rakenteeltaan puu.

Jos oletetaan, että jäsennettävän datan määrä on äärellinen ja muistia
jäsentämiseen on aina riittävästi, niin jäsennys aina päättyy onnistuneesti tai
epäonnistuneesti. Jokaisen tietorakenteen esittäminen vaatii aina jonkun verran
tilaa, ja tilaa voidaan rajallisesti jakaa tietorakenteiden kesken. Siispä
jokaisen dokumentin pohjimmaisten kerroksien tietorakenteiden on oltava
atomisia, eli ne eivät enää jakaudu uusiin kerroksiin, jotta jäsennys päättyisi
onnistuneesti. Jos tietorakennetta ei esitetä täydellisesti, saadaan aikaan
syntaksivirhe, ja jäsennys päättyy epäonnistuneesti.

Kieli ei sisällä yhtään määritelmää, joissa määritelmän vasemmanpuoleisin osa
olisi määritelmä itse. Toisin sanoen kielessä ei esiinny vasemmanpuoleista
rekursiota. Siispä myöskään kyseistä erityistapausta ei tarvitse huomioida
abstraktia syntaksipuuta muodostaessa. Kielessä jokainen rinnakkainen
päätemerkki poikkeaa toisistaan siten, että jäsennettävä data on aina
yksiselitteistä.

Datan puumaisen rakenteen ja yksiselitteisyyden, ongelmallisten rekursioiden
puutteen sekä kielen kontekstittomuuden vuoksi jäsennys ei voi jäädä ikuisesti
jäsentämään. Siispä jäsennyksen on aina päätyttävä joko onnistuneeseen tai
epäonnistuneeseen jäsennykseen. Kielelle toinen olennainen ratkeavuuskysymys
on, että saadaanko syötetystä tekstidokumentista onnistuneesti jäsennettyä
abstrakti syntaksipuu tai tekstiseikkailupeli.

# Pelin komentokieli

Jäsennettyä tekstiseikkailupeliä voi ohjata tekstipohjaisilla komennoilla,
jotka koostuvat seuraavanlaisesta ABNF:stä:

    command = go / pick_up / look / quit
    go = "go" *VCHAR
    pick_up = "pick up" *VCHAR
    quit = "quit"
    look = "look" (look_at / "around")
    look_at = "at" (inventory / *VCHAR)

Ohjelman lähdekoodissa jäsentäminen kuitenkin tapahtuu tavalla, joka muistuttaa
seuraavaa ABNF-säännöstöä:

    command = go / pick_up / look_at_inventory
            / look_at / look_around / quit
    go = "go" *VCHAR
    pick_up = "pick up" *VCHAR
    look_at_inventory = "look at inventory"
    look_at = "look at" *VCHAR
    look_around = "look around"
    quit = "quit"

Molemmat säännöstöt jäsentävät saman kielen, mutta jälkimmäisessä säännöstössä
eri katsomiset jäsennetään hieman epäoptimaalisemmalla tavalla. Ohjelmaan
valittiin jälkimmäinen säännöstö, koska se on miellekkäämpää ilmaista
ohjelmakoodissa kuin ensimmäinen säännöstö.

Komentokielen jäsentäjä jäsentää komentodatan suoraan merkkijonoista
käyttämällä samoja jäsentämisen työkaluja kuin mitä käytetään pelin abstraktin
syntaksipuun jäsentämiseen. Kuten pelin rakennetta kuvaavassa kielessä,
myöskään komentokielessä ei esiinny vasemmanpuoleista rekursiota, joten
kyseistä erityistapausta ei tarvitse huomioida jäsennintä muodostaessa.

Ohjelmassa komennot "look at inventory" ja "look at foobar" tulkitaan eri
komennoiksi. Ensimmäinen komento on pelaajan esineiden tarkastelua varten, ja
jälkimmäinen komento on yksittäisen esineen tarkastelua varten. Tällöin
ensimmäisen komennon on oltava etusijalla jäsentämisvaiheessa, jotta kyseinen
komento olisi mahdollista jäsentää tekstistä. Tällöin myös suljetaan pois
mahdollisuus komentoon, jolla haluttaisiin tarkastella "inventory" nimistä
esinettä.

Komentokieli ei sisällä yhtään rekursiivista määritelmää, se on kontekstiton,
ei sisällä vasemmanpuoleista rekursiota, ja sen jäsentäjässä on huomioitu
mahdolliset eri erikoistapaukset yksiselitteisyyden säilyttämiseksi. Siispä
jäsentämisen on pelin rakennetta kuvaavan kielen jäsentämisen tapaan päätyttävä
aina hyväksyttävään tai hylkäävään tilaan. Komentokielen toinen olennainen
ratkeavuuskysymys on, että saadaanko pelaajan syöttämästä merkkijonokomennosta
tulkittua pelikomento eli kuuluuko syötetty merkkijono komentokieleen.

# Tekstiseikkailupelin rakenne automaattina

Tekstidokumentista generoitua peliä ei missään vaiheessa ohjelmaa simuloida
kuten automaattia, mutta se voidaan mieltää eräänlaiseksi automaattien
yhdistelmäksi. Jokaista pelin huonetta kohden voidaan kuvitella olevan oma
automaatti, joka simuloi pelaajan käyttäytymistä huoneen sisällä. Lisäksi
pelissä on oltava automaatti, joka simuloi pelaajan liikkumista huoneiden
välillä.

Liikkumista simuloivassa automaatissa, eli niin sanotussa
huoneistoautomaatissa, automaatin tiloina toimivat huoneet. Huoneista
alkutilana toimii pelin aloitushuone. Koska esineet eivät vaikuta pelaajan
kykyyn liikkua huoneista toiseen, huoneistoautomaatin syötteet koostuvat
pelkästään pelaajan syötteistä. Pelaajan syötteistä ainoastaan
siirtymiskomennot vaikuttavat pelaajan siirtymiseen huoneesta toiseen. Muut
komennot palauttavat pelaajan samaan tilaan.

Huoneen sisäisessä automaatissa, eli niin sanotussa huoneautomaatissa,
automaatin tilat koostuvat kaikista mahdollisista huoneen esineiden poimintojen
kombinaatioista. Esimerkiksi jos huone sisältää useamman poimittavan esineen,
niin automaatin tilojen täytyy kattaa kaikki mahdolliset kombinaatiot missä
järjestyksessä esineet voidaan poimia. Alkutilana toimii tila, jossa yhtään
huoneen esinettä ei ole vielä poimittu.

Pelaajan olemassa olevat esineet eivät vaikuta mahdollisiin poimintoihin, joten
huoneautomaatissa automaatin syötteet koostuvat ainoastaan pelaajan syötteistä.
Kun pelaaja poimii esineen, tapahtuu siirtymä tilaan, jossa kyseinen esine on
poimittu. Tästä tilasta löytyy siirtymät tiloihin, joissa seuraavat esineet
poimitaan. Kaikilla muilla syötteillä kuin poimimiskomennoilla pelaaja siirtyy
samaan tilaan.

Koska esineiden on säilyttävä poimittuina pelaajan siirtyessä huoneiden
välillä, huoneautomaattien tilojen on säilyttävä muistissa. Siispä huoneisto-
ja huoneautomaatteja ei voida yhdistää yhdeksi äärelliseksi automaatiksi.
Huoneiden simuloiminen automaateilla on muutenkin tässä tapauksessa turhan
monimutkaista, koska huoneista tarvitsee vain poimia esineitä pelaajan
tavaroiden sekaan ilman, että tarvitsee välittää esineiden
poimimisjärjestyksestä.

# Muut kokemukset ja haasteet

Haskell-ohjelmointikielen valinta osoittautui oivalliseksi valinnaksi
jäsentimien tekemiseen. Haskell tarjoaa runsaasti valmiita korkean tason
kirjastoja jäsentimien hallintaan, joilla jäsentäminen onnistui täsmäkieliä
muistuttavalla tavalla. Myös datan poimiminen abstraktin syntaksi puun joukosta
onnistui kätevästi käyttämällä Haskellin tyyppiluokka-ominaisuuksia.

Harjoitustyössä toteutettu jäsennin monadi, joko jäsentää datan onnistuneesti
tai epäonnistuu jäsentämisessä. Jäsentämisessä tapahtuneiden virheiden tietoja
ei missään kerätä talteen, minkä vuoksi jäsentämisestä kerättävät
virheilmoitukset jäävät hyvin vähäisiksi.

Koska käsiteltävä data tyypillisesti sisältää syklisiä viitteitä (esimerkiksi
huoneiden väliset viitteet), käsiteltävästä datasta muodostuu graafi. Graafin
muodostaminen osoittautui Haskellissa ongelmalliseksi johtuen Haskellin
muuttumattomista tietorakenteista. Esimerkiksi jos data A sisältää viitteen
dataan B, niin tällöin B on rakennettava ennen dataa A. Jos nyt data B sisältää
vielä viitteen dataan A, niin B:n on oltava rakennettuna ennen dataa A, jolloin
saadaan aikaiseksi täysin mahdoton riippuvuusketju. Ohjelmassa ongelma
ratkaistiin siten, että jokainen datan sisältämä viite on vain avain, jota
vasten on mahdollista myöhemmin hakea erillisestä hajautustaulusta viitettä
vastaava data. Ratkaisun huonona puolena on, että kyseisiä hajautustauluja on
kuljetettava funktioille kun halutaan kaivaa dataa viitteiden takaa.

[tagenerator]: https://github.com/jkpl/tagenerator
[Haskell]: http://www.haskell.org/haskellwiki/Haskell
