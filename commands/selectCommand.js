const { parseColumnNames } = require('./parserTools/parseColumnNames')
const { SelectSchema } = require('../models/SelectSchema')

const parseCommand = (fullCommandAsStringList) => {
    //KOMENTO
    let parsedCommand = {
        name: fullCommandAsStringList[0],
        size: fullCommandAsStringList.length,
        parserCounter: 1,
    }

    // SARAKKEIDEN OSIO - (*AS -- TODO*)
    parsedCommand = parseColumnNames(fullCommandAsStringList, parsedCommand)

    //FROM  (* etsiminen muualtakin -- TODO*)
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
                fullCommandAsStringList.length - 1
            )
            .join(' ')
    } else {
        parsedCommand.unparsedBeforeFinalSemicolon = fullCommandAsStringList
            .slice(parsedCommand.parserCounter, fullCommandAsStringList.length)
            .join(' ')
    }
    return SelectSchema.validate(parsedCommand)
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
