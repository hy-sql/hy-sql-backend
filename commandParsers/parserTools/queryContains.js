const queryContainsWhereKeyword = (fullCommandAsStringArray) => {
    const where = fullCommandAsStringArray.findIndex(
        (string) => string.toUpperCase() === 'WHERE'
    )
    return where !== -1
}

const queryContainsOrderByKeywords = (fullCommandAsStringArray) => {
    const hasOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    const hasBy = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'BY'
    )

    return hasOrder > 0 && hasBy > 0 ? hasOrder < hasBy : false
}

const queryContainsWhereOrderByKeywords = (fullCommandAsStringArray) => {
    const hasWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )
    const hasOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )
    const hasBy = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'BY'
    )

    return hasWhere > 0 && hasOrder > 0 && hasBy > 0
        ? hasWhere < hasOrder && hasOrder < hasBy
        : false
}

module.exports = {
    queryContainsWhereKeyword,
    queryContainsOrderByKeywords,
    queryContainsWhereOrderByKeywords,
}
