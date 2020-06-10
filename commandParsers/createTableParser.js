/* eslint-disable no-unused-vars */
const { CreateTableSchema } = require('../schemas/CreateTableSchema')
const { constraintsNamePatternForSplit } = require('../helpers/regex')

const parseCommand = (fullCommandAsStringArray) => {
    const parsedCommand = {
        name: fullCommandAsStringArray.slice(0, 2).join(' '),
        tableName: fullCommandAsStringArray[2],
        openingBracket: fullCommandAsStringArray[3],
        columns: parseColumns(
            fullCommandAsStringArray.slice(
                fullCommandAsStringArray.indexOf('(') + 1,
                fullCommandAsStringArray.indexOf(')')
            )
        ),
        closingBracket: fullCommandAsStringArray.join(' ').indexOf(')') > 0,
        finalSemicolon:
            fullCommandAsStringArray[fullCommandAsStringArray.length - 1] ===
            ';'
                ? ';'
                : undefined,
    }

    return CreateTableSchema.validate(parsedCommand)
}

const parseColumns = (columnsAsStringList) => {
    if (!columnsAsStringList) return null

    const separatedColumnsAsStringList = columnsAsStringList
        .join(' ')
        .split(', ')

    const columns = separatedColumnsAsStringList
        .map((c) => c.split(' '))
        .map((item) => {
            return {
                name: item[0],
                type: item[1] ? item[1].toUpperCase() : null,
                constraints: parseColumnConstraints(item.slice(2)),
            }
        })

    return columns
}

const parseColumnConstraints = (constraintsAsStringArray) => {
    const separatedConstraintsAsStringList = constraintsAsStringArray
        .join(' ')
        .toUpperCase()
        .split(constraintsNamePatternForSplit)
        .map((c) => c.trim())
        .filter(Boolean)

    return separatedConstraintsAsStringList
}

module.exports = { parseCommand }
