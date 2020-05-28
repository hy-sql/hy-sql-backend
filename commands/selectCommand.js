const { parseColumnNames } = require('./parserTools/parseColumnNames')

const selectSchema = require('../models/SelectSchema')

const parseCommand = (fullCommandAsStringList) => {
    //KOMENTO
    let parsedCommand = {
        name: fullCommandAsStringList[0],
        size: fullCommandAsStringList.length,
        parserCounter: 1,
    }

    // SARAKKEIDEN OSIO - (*AS -- TODO*)
    parsedCommand = parseColumnNames(fullCommandAsStringList, parsedCommand)

    if (parsedCommand.columnsOpenBrackets === 0)
        delete parsedCommand.columnsOpenBrackets
    //FROM
    //tämä olisi helppo muuttaa käymään koko rimpsua läpi, palauttamaan avainsanan, ja heittämään virheen jos avainsana on väärässä paikassa
    //vs nykyinen, missä katsotaan vain onko se FROM siinä oikeassa paikassa
    if (
        fullCommandAsStringList[parsedCommand.parserCounter].toUpperCase() ===
        'FROM'
    ) {
        parsedCommand.from =
            fullCommandAsStringList[parsedCommand.parserCounter]
        parsedCommand.parserCounter++
    }

    //TAULUJEN OSIO
    parsedCommand = parseTableNames(fullCommandAsStringList, parsedCommand)

    // WHERE OSIO - specifies which rows to retrieve.

    // GROUP BY - groups rows sharing a property so that an aggregate function can be applied to each group.

    // HAVING - selects among the groups defined by the GROUP BY clause.

    // ORDER BY -- tehdään controllerissa -- specifies an order in which to return the rows.

    // finalSemicolon ---- NÄMÄ PITÄÄ SIISTIÄ, HOLY SPAGHETTI BATMAN
    if (fullCommandAsStringList[parsedCommand.parserCounter] === ';') {
        parsedCommand.finalSemicolon = ';'
        parsedCommand.parserCounter++
    } else if (
        fullCommandAsStringList[fullCommandAsStringList.length - 1] === ';'
    ) {
        parsedCommand.finalSemicolon = ';'
        parsedCommand.unparsedBeforeFinalSemicolon = fullCommandAsStringList
            .slice(
                parsedCommand.parserCounter,
                fullCommandAsStringList.length - 2
            )
            .join(' ')
    } else {
        parsedCommand.unparsedBeforeFinalSemicolon = fullCommandAsStringList
            .slice(
                parsedCommand.parserCounter,
                fullCommandAsStringList.length - 1
            )
            .join(' ')
    }
    console.log('ennen validointia', parsedCommand)
    return selectSchema.validate(parsedCommand)
}

const parseTableNames = (stringArray, parsedCommand) => {
    //näitä lisää, mieluiten johonkin ReservedWords-listaan joka importataan, *TODO: RESERVED WORDS LIST*
    if (
        !['WHERE', 'JOIN', '(', ')', ';', 'VALUES'].includes(
            stringArray[parsedCommand.parserCounter].toUpperCase()
        )
    ) {
        parsedCommand.tableName = stringArray[parsedCommand.parserCounter]
        parsedCommand.parserCounter++
    }
    return parsedCommand
}

module.exports = { parseCommand }
