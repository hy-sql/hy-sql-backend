const { cleanStringArray } = require('./parserStringCleaners')

const selectSchema = require('../models/SelectSchema')

const parseCommand = (fullCommandAsStringList) => {
    // ensin simppeli ilman alikyselyitä
    console.log(fullCommandAsStringList)

    const parsedCommand = {
        name: fullCommandAsStringList[0],
    }
    //const columns = []

    // SARAKKEIDEN OSIO - (*AS -- TODO*)
    let jatkavaLaskuri = 1
    let laskuri = 1
    let avoimetSulut = 0
    loop1: while (laskuri < fullCommandAsStringList.length) {
        //switch tiätty tähän
        console.log(fullCommandAsStringList[laskuri])
        switch (fullCommandAsStringList[laskuri].toUpperCase()) {
            case '(':
                avoimetSulut++
                laskuri++
                continue loop1
            case 'JOIN':
                //katsotaan avoimet sulut, tarvitseeko välittää, SITTEN ihmetellään mikä otus kyseessä
                if (avoimetSulut === 0) {
                    //heitetään varmaan joku virhe koska on mahdoton sanoa
                    //onko edellisessä termissä kyseessä taulu vai sarake, tuo [laskuri -1 ] on se epäselvä
                    //tarkistus vielä ettei olla komentoa seuraavassa sanassa, pitää keksiä jotain!
                    break loop1
                }
                laskuri++
                continue loop1
            //muut katkaisevat sanat
            case 'GROUP BY':
            case 'HAVING':
            case 'WHERE':
            case 'FROM':
                if (avoimetSulut === 0) break loop1
            /* falls through */
            case ')':
                if (avoimetSulut > 0) avoimetSulut-- //tää kai suoritetaan vain jos on tuo, eli voin käyttää taas sitä pinoamista
            /* falls through */
            default:
                laskuri++
        }
    }
    if (laskuri === fullCommandAsStringList.length) {
        console.log(
            'laskimena käytetään, tai muuten ilman tauluja, tai unohtunut FROM'
        )
        return 'LASKIN MODE - TODO'
    }
    //eli löytyi tuo, voidaan asettaa jatkavaLaskuri tähän kohtaan
    jatkavaLaskuri = laskuri
    console.log(jatkavaLaskuri)

    parsedCommand.columns = cleanStringArray(
        fullCommandAsStringList.slice(1, laskuri)
    ).map((c) => {
        return { name: c }
    })
    parsedCommand.fromKeyword = fullCommandAsStringList[laskuri]

    //TAULUJEN OSIO
    //tableName: sana

    // WHERE OSIO - specifies which rows to retrieve.

    // GROUP BY - groups rows sharing a property so that an aggregate function can be applied to each group.

    // HAVING - selects among the groups defined by the GROUP BY clause.

    // ORDER BY -- tehdään controllerissa -- specifies an order in which to return the rows.

    // finalSemicolon

    console.log(parsedCommand)
    return selectSchema.validate(parsedCommand)
}

module.exports = { parseCommand }
