/* eslint-disable no-unused-vars */
const { CreateTableSchema } = require('../schemas/CreateTableSchema')

const parseCommand = (fullCommandAsStringList) => {
    const parsedCommand = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        tableName: fullCommandAsStringList[2],
        openingBracket: fullCommandAsStringList[3],
        columns: parseColumns(
            fullCommandAsStringList.slice(
                fullCommandAsStringList.indexOf('(') + 1,
                fullCommandAsStringList.indexOf(')')
            )
        ),
        closingBracket: fullCommandAsStringList.join(' ').indexOf(')') > 0,
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1] === ';'
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
    const { constraintsNamePatternForSplit } = require('../helpers/regex')

    const separatedConstraintsAsStringList = constraintsAsStringArray
        .join(' ')
        .toUpperCase()
        .split(constraintsNamePatternForSplit)
        .map((c) => c.trim())
        .filter(Boolean)

    return separatedConstraintsAsStringList
}

module.exports = { parseCommand }
