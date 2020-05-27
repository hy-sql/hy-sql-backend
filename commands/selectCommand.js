const { parseColumnNames } = require('./parserTools/parseColumnNames')

const selectSchema = require('../models/SelectSchema')

const parseCommand = (fullCommandAsStringList) => {
    //KOMENTO
    const parsedCommand = {
        name: fullCommandAsStringList[0],
    }
    let parserCounter = 1

    // SARAKKEIDEN OSIO - (*AS -- TODO*)
    let palautusolio = parseColumnNames(parserCounter, fullCommandAsStringList)
    parserCounter = palautusolio.parserCounter
    palautusolio.columnsOpenBrackets > 0
        ? parsedCommand.columnsOpenBrackets
        : null
    parsedCommand.columns = palautusolio.columns

    //FROM
    if (fullCommandAsStringList[parserCounter].toUpperCase() === 'FROM') {
        parsedCommand.from = fullCommandAsStringList[parserCounter]
        parserCounter++
    }

    //TAULUJEN OSIO
    palautusolio = parseTableNames(parserCounter, fullCommandAsStringList)
    parserCounter = palautusolio.parserCounter
    if (palautusolio.tableName) parsedCommand.tableName = palautusolio.tableName
    // WHERE OSIO - specifies which rows to retrieve.

    // GROUP BY - groups rows sharing a property so that an aggregate function can be applied to each group.

    // HAVING - selects among the groups defined by the GROUP BY clause.

    // ORDER BY -- tehdään controllerissa -- specifies an order in which to return the rows.

    // finalSemicolon
    if (fullCommandAsStringList[parserCounter] === ';')
        parsedCommand.finalSemicolon = ';'

    return selectSchema.validate(parsedCommand)
}

const parseTableNames = (parserCounter, stringArray) => {
    let palautettava = {
        parserCounter,
    }
    if (stringArray[parserCounter].toUpperCase() !== 'WHERE|JOIN|(|)') {
        palautettava.parserCounter++
        palautettava.tableName = stringArray[parserCounter]
    }
    return palautettava
}

module.exports = { parseCommand }
