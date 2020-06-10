const { parseColumnNames } = require('./parserTools/parseColumnNames')
const {
    parseWhereToCommandObject,
    queryContainsWhereKeyword,
} = require('./whereCommand')
const {
    parseOrderBy,
    hasOrderByKeywords,
    hasWhereOrderByKeywords,
} = require('./orderByParser')
const {
    SelectSchema,
    SelectColumnsWhereSchema,
    SelectColumnsOrderBySchema,
    SelectColumnsWhereOrderBySchema,
} = require('../schemas/SelectSchema')

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

const parseSelectColumns = (fullCommandAsStringArray) => {
    //KOMENTO
    let parsedCommand = {
        name: fullCommandAsStringArray[0],
        size: fullCommandAsStringArray.length,
        parserCounter: 1,
    }

    // SARAKKEIDEN OSIO - (*AS -- TODO*)
    //return { pccolumns: laskuri, columns: namify(cleanStringArray(columns)) }
    const { pccolumns, columns } = parseColumnNames(
        fullCommandAsStringArray,
        parsedCommand.parserCounter
    )
    if (columns) {
        parsedCommand.parserCounter = pccolumns
        parsedCommand.columns = columns
    }

    //FROM  (* etsiminen muualtakin -- TODO*)
    if (
        fullCommandAsStringArray[parsedCommand.parserCounter].toUpperCase() ===
        'FROM'
    ) {
        parsedCommand.from =
            fullCommandAsStringArray[parsedCommand.parserCounter]
        parsedCommand.parserCounter++
    }

    //TAULUJEN OSIO
    const { pctable, tableName } = parseTableNames(
        fullCommandAsStringArray,
        parsedCommand.parserCounter
    )

    if (tableName) {
        parsedCommand.parserCounter = pctable
        parsedCommand.tableName = tableName
    }
    // WHERE OSIO - specifies which rows to retrieve.

    // GROUP BY - groups rows sharing a property so that an aggregate function can be applied to each group.

    // HAVING - selects among the groups defined by the GROUP BY clause.

    // ORDER BY -- tehdään controllerissa -- specifies an order in which to return the rows.

    // finalSemicolon ---- NÄMÄ PITÄÄ SIISTIÄ, HOLY SPAGHETTI BATMAN
    if (fullCommandAsStringArray[parsedCommand.parserCounter] === ';') {
        parsedCommand.finalSemicolon = ';'
        parsedCommand.parserCounter++
    } else if (
        fullCommandAsStringArray[fullCommandAsStringArray.length - 1] === ';'
    ) {
        parsedCommand.finalSemicolon = ';'
        parsedCommand.unparsedBeforeFinalSemicolon = fullCommandAsStringArray.slice(
            parsedCommand.parserCounter,
            fullCommandAsStringArray.length - 1
        )
    } else {
        parsedCommand.unparsedBeforeFinalSemicolon = fullCommandAsStringArray.slice(
            parsedCommand.parserCounter,
            fullCommandAsStringArray.length
        )
    }

    const validationResult = SelectSchema.validate(parsedCommand)

    return validationResult
}

const parseTableNames = (stringArray, parserCounter) => {
    //näitä lisää, mieluiten johonkin ReservedWords-listaan joka importataan, *TODO: RESERVED WORDS LIST*
    if (
        !['WHERE', 'JOIN', '(', ')', ';', 'VALUES', 'ORDER', 'GROUP'].includes(
            stringArray[parserCounter].toUpperCase()
        )
    ) {
        const tableName = stringArray[parserCounter]
        parserCounter++
        return { pctable: parserCounter, tableName: tableName }
    }
    return { pctable: parserCounter }
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
