const { parseColumnNames } = require('./parserTools')

const selectSchema = require('../models/SelectSchema')

const parseCommand = (fullCommandAsStringList) => {
    // ensin simppeli ilman alikyselyitä
    console.log(fullCommandAsStringList)

    //KOMENTO
    const parsedCommand = {
        name: fullCommandAsStringList[0],
    }
    let parserCounter = 1

    // SARAKKEIDEN OSIO - (*AS -- TODO*)
    let palautusolio = parseColumnNames(parserCounter, fullCommandAsStringList)
    parserCounter = palautusolio.parserCounter //jatketaan tästä seuraavassa
    palautusolio.columnsOpenBrackets > 0 //tarkoituksena että heittää errorin validoinnissa
        ? parsedCommand.columnsOpenBrackets
        : null
    parsedCommand.columns = palautusolio.columns

    //FROM
    if (fullCommandAsStringList[parserCounter].toUpperCase() === 'FROM') {
        parsedCommand.fromKeyword = fullCommandAsStringList[parserCounter]
        parserCounter++
    }

    //TAULUJEN OSIO
    palautusolio = parseTableNames(parserCounter, fullCommandAsStringList)

    // WHERE OSIO - specifies which rows to retrieve.

    // GROUP BY - groups rows sharing a property so that an aggregate function can be applied to each group.

    // HAVING - selects among the groups defined by the GROUP BY clause.

    // ORDER BY -- tehdään controllerissa -- specifies an order in which to return the rows.

    // finalSemicolon

    console.log(parsedCommand)
    return selectSchema.validate(parsedCommand)
}

const parseTableNames = (parserCounter, stringArray) => {
    console.log(stringArray)
}

module.exports = { parseCommand }
