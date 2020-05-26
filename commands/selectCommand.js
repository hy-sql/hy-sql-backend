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
