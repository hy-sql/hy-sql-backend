const queryContainsWhereKeyword = (fullCommandAsStringArray) => {
    return fullCommandAsStringArray.some(
        (string) => string.toUpperCase() === 'WHERE'
    )
}

const queryContainsGroupByKeywords = (fullCommandAsStringArray) => {
    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    if (indexOfGroup < 0) return false

    const hasGroupBy =
        indexOfGroup >= 0 &&
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY'

    return hasGroupBy
}

const queryContainsOrderByKeywords = (fullCommandAsStringArray) => {
    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    if (indexOfOrder < 0) return false

    const hasOrderBy =
        indexOfOrder >= 0 &&
        fullCommandAsStringArray[indexOfOrder + 1] &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY'

    return hasOrderBy
}

const queryContainsWhereGroupByKeywords = (fullCommandAsStringArray) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    if (indexOfWhere < 0 || indexOfGroup < 0) return false

    const hasGroupBy =
        indexOfGroup >= 0 &&
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY'

    return indexOfWhere >= 0 && indexOfWhere < indexOfGroup && hasGroupBy
}

const queryContainsGroupByOrderByKeywords = (fullCommandAsStringArray) => {
    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    if (indexOfGroup < 0 || indexOfOrder < 0) return false

    const hasGroupBy =
        indexOfGroup >= 0 &&
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY'

    const hasOrderBy =
        indexOfOrder >= 0 &&
        fullCommandAsStringArray[indexOfOrder + 1] &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY'

    return indexOfGroup < indexOfOrder && hasGroupBy && hasOrderBy
}

const queryContainsWhereOrderByKeywords = (fullCommandAsStringArray) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    if (indexOfWhere < 0 || indexOfOrder < 0) return false

    const hasOrderBy =
        indexOfOrder >= 0 &&
        fullCommandAsStringArray[indexOfOrder + 1] &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY'

    return indexOfWhere >= 0 && indexOfWhere < indexOfOrder && hasOrderBy
}

const queryContainsWhereGroupByOrderByKeywords = (fullCommandAsStringArray) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    if (indexOfWhere < 0 || indexOfGroup < 0 || indexOfOrder < 0) return false

    const hasGroupBy =
        indexOfGroup >= 0 &&
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY'

    const hasOrderBy =
        indexOfOrder >= 0 &&
        fullCommandAsStringArray[indexOfOrder + 1] &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY'

    return (
        indexOfWhere >= 0 &&
        indexOfWhere < indexOfGroup &&
        indexOfGroup < indexOfOrder &&
        hasGroupBy &&
        hasOrderBy
    )
}

module.exports = {
    queryContainsWhereKeyword,
    queryContainsGroupByKeywords,
    queryContainsOrderByKeywords,
    queryContainsWhereGroupByKeywords,
    queryContainsWhereOrderByKeywords,
    queryContainsGroupByOrderByKeywords,
    queryContainsWhereGroupByOrderByKeywords,
}
