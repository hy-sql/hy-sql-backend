const { parseColumnNames } = require('./parserTools')

//const selectSchema = require('../models/SelectSchema')

const parseCommand = (fullCommandAsStringList) => {
    // ensin simppeli ilman alikyselyitä
    console.log(fullCommandAsStringList)

    const parsedCommand = {
        name: fullCommandAsStringList[0],
    }
    let parserCounter = 1
    //const columns = []

    // SARAKKEIDEN OSIO - (*AS -- TODO*)
    //eli löytyi tuo, voidaan asettaa jatkavaLaskuri tähän kohtaan
    //    let index = 1
    let palautusolio = parseColumnNames(parserCounter, fullCommandAsStringList)
    console.log('palautusolio:', palautusolio)
    parserCounter = palautusolio.parserCounter
    palautusolio.columnsOpenBrackets > 0
        ? parsedCommand.columnsOpenBrackets
        : null
    parsedCommand.columns = palautusolio.columns

    /*

        parsedCommand.columns = cleanStringArray(
            fullCommandAsStringList.slice(1, laskuri)
        ).map((c) => {
            return { name: c }
        })
        parsedCommand.fromKeyword = fullCommandAsStringList[laskuri]*/

    //TAULUJEN OSIO
    //tableName: sana

    // WHERE OSIO - specifies which rows to retrieve.

    // GROUP BY - groups rows sharing a property so that an aggregate function can be applied to each group.

    // HAVING - selects among the groups defined by the GROUP BY clause.

    // ORDER BY -- tehdään controllerissa -- specifies an order in which to return the rows.

    // finalSemicolon

    console.log(parsedCommand)
    //return selectSchema.validate(parsedCommand)
    return parsedCommand
}

module.exports = { parseCommand }
