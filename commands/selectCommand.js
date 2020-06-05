const { parseColumnNames } = require('./parserTools/parseColumnNames')
const {
    parseWhereToCommandObject,
    queryContainsWhereKeyword,
} = require('./whereCommand')
const {
    parseOrderBy,
    hasOrderByKeywords,
    hasWhereOrderByKeywords,
} = require('./orderByCommand')
const { SelectSchema } = require('../models/SelectSchema')
const {
    SelectColumnsWhereSchema,
    SelectColumnsOrderBySchema,
    SelectColumnsWhereOrderBySchema,
} = require('../models/SelectSchema')

const parseCommand = (fullCommandAsStringArray) => {
    if (hasWhereOrderByKeywords(fullCommandAsStringArray)) {
        return parseSelectColumnsWhereOrderBy(fullCommandAsStringArray)
    } else if (hasOrderByKeywords(fullCommandAsStringArray)) {
        return parseSelectColumnsOrderBy(fullCommandAsStringArray)
    } else if (queryContainsWhereKeyword(fullCommandAsStringArray)) {
        return parseSelectColumnsWhere(fullCommandAsStringArray)
    }

    return parseSelectColumns(fullCommandAsStringArray)
}

const parseSelectColumns = (fullCommandAsStringList) => {
    //KOMENTO
    let parsedCommand = {
        name: fullCommandAsStringList[0],
        size: fullCommandAsStringList.length,
        parserCounter: 1,
    }

    // SARAKKEIDEN OSIO - (*AS -- TODO*)
    //return { pccolumns: laskuri, columns: namify(cleanStringArray(columns)) }
    const { pccolumns, columns } = parseColumnNames(
        fullCommandAsStringList,
        parsedCommand.parserCounter
    )
    if (columns) {
        parsedCommand.parserCounter = pccolumns
        parsedCommand.columns = columns
    }

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
        parsedCommand.unparsedBeforeFinalSemicolon = fullCommandAsStringList.slice(
            parsedCommand.parserCounter,
            fullCommandAsStringList.length - 1
        )
    } else {
        parsedCommand.unparsedBeforeFinalSemicolon = fullCommandAsStringList.slice(
            parsedCommand.parserCounter,
            fullCommandAsStringList.length
        )
    }

    const validationResult = SelectSchema.validate(parsedCommand)

    return validationResult
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

const parseSelectColumnsWhere = (fullCommandAsStringArray) => {
    const parsedBaseCommand = parseSelectColumns(fullCommandAsStringArray)

    parsedBaseCommand.value.where = parseWhereToCommandObject(
        parsedBaseCommand.value.unparsedBeforeFinalSemicolon
    )

    /* + 1 lisätty koska osassa valideja kyselyitä parserCounter palauttaa yhtä pienemmän luvun kuin tulisi,
      jolloin kyselyn validaatio hylkää kyseisen kyselyn väärin perustein.
      Jos parserCounterin laskutapaa muutetaan voi +1 todennäköisesti poistaa tarpeettomana*/
    parsedBaseCommand.value.parserCounter =
        parsedBaseCommand.value.parserCounter +
        parsedBaseCommand.value.where.indexCounter +
        1

    delete parsedBaseCommand.value.unparsedBeforeFinalSemicolon

    const validationResult = SelectColumnsWhereSchema.validate(
        parsedBaseCommand.value
    )

    return validationResult
}

const parseSelectColumnsOrderBy = (fullCommandAsStringArray) => {
    const parsedBaseCommand = parseSelectColumns(fullCommandAsStringArray)

    parsedBaseCommand.value.orderBy = parseOrderBy(
        parsedBaseCommand.value.unparsedBeforeFinalSemicolon
    )

    delete parsedBaseCommand.value.unparsedBeforeFinalSemicolon
    delete parsedBaseCommand.value.parserCounter

    const validationResult = SelectColumnsOrderBySchema.validate(
        parsedBaseCommand.value
    )

    return validationResult
}

const parseSelectColumnsWhereOrderBy = (fullCommandAsStringArray) => {
    const parsedBaseCommand = parseSelectColumns(fullCommandAsStringArray)

    const unparsed = parsedBaseCommand.value.unparsedBeforeFinalSemicolon

    const indexOfOrder = unparsed.findIndex((c) => c.toUpperCase() === 'ORDER')

    parsedBaseCommand.value.where = parseWhereToCommandObject(
        unparsed.slice(0, indexOfOrder)
    )

    parsedBaseCommand.value.orderBy = parseOrderBy(unparsed.slice(indexOfOrder))

    delete parsedBaseCommand.value.unparsedBeforeFinalSemicolon
    delete parsedBaseCommand.value.parserCounter
    delete parsedBaseCommand.value.where.indexCounter

    const validationResult = SelectColumnsWhereOrderBySchema.validate(
        parsedBaseCommand.value
    )

    return validationResult
}

module.exports = { parseCommand }
