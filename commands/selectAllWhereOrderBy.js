const { parseOrderBy } = require('./selectAllCommand')
const { parseWhereToCommandObject } = require('./whereCommand')

// eslint-disable-next-line no-unused-vars
const parseSelectAllWhere = (fullCommandAsStringList) => {
    if (hasWhereOrderBy(fullCommandAsStringList)) {
        return parseSelectWhereOrderBy(fullCommandAsStringList)
    }
}

const parseSelectWhereOrderBy = (fullCommandAsStringList) => {
    const indexOfOrder = fullCommandAsStringList.findIndex(
        (c) => c.toUpperCase() === 'ORDER'
    )

    const parsedCommand = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        from: fullCommandAsStringList[2],
        tableName: fullCommandAsStringList[3],
        where: parseWhereToCommandObject(
            fullCommandAsStringList.slice(4, indexOfOrder)
        ),
        orderBy: parseOrderBy(
            fullCommandAsStringList.slice(
                indexOfOrder,
                fullCommandAsStringList.length - 1
            )
        ),
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1] === ';'
                ? ';'
                : undefined,
    }

    console.log(parsedCommand)
}

const hasWhereOrderBy = (fullCommandAsStringList) => {
    const hasWhere = fullCommandAsStringList.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )
    const hasOrder = fullCommandAsStringList.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )
    const hasBy = fullCommandAsStringList.findIndex(
        (s) => s.toUpperCase() === 'BY'
    )

    return hasWhere > 0 && hasOrder > 0 && hasBy > 0
        ? hasWhere < hasOrder && hasOrder < hasBy
        : false
}

const input = 'SELECT * FROM Tuotteet WHERE nimi=\'retiisi\' ORDER BY hinta;'

const splitCommand = input
    .trim()
    .replace(/\s\s+/g, ' ')
    .replace(/\s+,/g, ',')
    .split(/[\s]|(?<=,)|(?<=\()|(?=\))|(;$)/)
    .filter(Boolean)

console.log(splitCommand)

const result = parseSelectWhereOrderBy(splitCommand)

console.log(result)
