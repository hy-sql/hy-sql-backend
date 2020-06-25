const {
    checkLimitPosition,
    checkGroupByPosition,
    checkOrderByPosition,
    checkWherePosition,
} = require('../parserTools/checkPosition')

/**
 * Checks whether a command contains the keyword WHERE and in the correct
 * position. Check is case-insensitive. Returns true or false.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} WHERE was found true/false
 */
const queryContainsWhereKeyword = (fullCommandAsStringArray) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )

    if (indexOfWhere !== -1) checkWherePosition(fullCommandAsStringArray)

    return indexOfWhere !== -1
}

/**
 * Checks whether a command contains the keyword GROUP BY and in the correct
 * position. Check is case-insensitive. Returns true or false.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} GROUP BY was found true/false
 */
const queryContainsGroupByKeyword = (fullCommandAsStringArray) => {
    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    if (indexOfGroup !== -1) checkGroupByPosition(fullCommandAsStringArray)

    return (
        indexOfGroup !== -1 &&
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY'
    )
}

/**
 * Checks whether a command contains the keywords GROUP, BY and HAVING and that they
 * are in the correct order. Check is case-insensitive. Returns true or false.
 * @param {any} fullCommandAsStringArray command as string array
 * @returns {Boolean} GROUP BY and HAVING were found true/false
 */
const queryContainsGroupByHavingKeywords = (fullCommandAsStringArray) => {
    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    const indexOfHaving = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'HAVING'
    )

    if (indexOfGroup < 0 || indexOfHaving < 0) return false

    const hasGroupBy =
        indexOfGroup >= 0 &&
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY'

    return indexOfGroup < indexOfHaving && hasGroupBy
}

/**
 * Checks whether a command contains the keyword ORDER BY and in the correct
 * position. Check is case-insensitive. Returns true or false.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} ORDER BY was found true/false
 */
const queryContainsOrderByKeyword = (fullCommandAsStringArray) => {
    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    if (indexOfOrder !== -1) checkOrderByPosition(fullCommandAsStringArray)

    return (
        indexOfOrder !== -1 &&
        fullCommandAsStringArray[indexOfOrder + 1] &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY'
    )
}

/**
 * Checks whether a command contains both keywords WHERE and GROUP BY and that they
 * are in the correct order. Check is case-insensitive. Returns true or false.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} WHERE and GROUP BY were found true/false
 */
const queryContainsWhereGroupByKeywords = (fullCommandAsStringArray) => {
    const containsWhere = queryContainsWhereKeyword(fullCommandAsStringArray)
    const containsGroupBy = queryContainsGroupByKeyword(
        fullCommandAsStringArray
    )

    return containsWhere && containsGroupBy
}

/**
 * Checks whether a command contains the keywords WHERE, GROUP, BY and HAVING and that they
 * are in the correct order. Check is case-insensitive. Returns true or false.
 * @param {any} fullCommandAsStringArray command as string array
 * @returns {Boolean} WHERE and GROUP BY and HAVING were found true/false
 */
const queryContainsWhereGroupByHavingKeywords = (fullCommandAsStringArray) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    const indexOfHaving = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'HAVING'
    )

    if (indexOfWhere < 0 || indexOfGroup < 0 || indexOfHaving < 0) return false

    const hasGroupBy =
        indexOfGroup >= 0 &&
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY'

    return (
        indexOfWhere >= 0 &&
        indexOfWhere < indexOfGroup &&
        indexOfGroup < indexOfHaving &&
        hasGroupBy
    )
}

/**
 * Checks whether a command contains both keywords GROUP BY and ORDER BY and that they
 * are in the correct order. Check is case-insensitive. Returns true or false.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} GROUP BY and ORDER BY were found true/false
 */
const queryContainsGroupByOrderByKeywords = (fullCommandAsStringArray) => {
    const containsGroupBy = queryContainsGroupByKeyword(
        fullCommandAsStringArray
    )
    const containsOrderBy = queryContainsOrderByKeyword(
        fullCommandAsStringArray
    )

    return containsGroupBy && containsOrderBy
}

/**
 * Checks whether a command contains the keywords GROUP, BY, HAVING, ORDER and BY and that they
 * are in the correct order. Check is case-insensitive. Returns true or false.
 * @param {any} fullCommandAsStringArray command as string array
 * @returns {Boolean} GROUP BY and HAVING and ORDER BY were found true/false
 */
const queryContainsGroupByHavingOrderByKeywords = (
    fullCommandAsStringArray
) => {
    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    const indexOfHaving = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'HAVING'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    if (indexOfGroup < 0 || indexOfHaving < 0 || indexOfOrder < 0) return false

    const hasGroupBy =
        indexOfGroup >= 0 &&
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY'

    const hasOrderBy =
        indexOfOrder >= 0 &&
        fullCommandAsStringArray[indexOfOrder + 1] &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY'

    return (
        indexOfGroup >= 0 &&
        indexOfGroup < indexOfHaving &&
        indexOfHaving < indexOfOrder &&
        hasGroupBy &&
        hasOrderBy
    )
}

/**
 * Checks whether a command contains both keywords WHERE and ORDER BY and that they
 * are in the correct order. Check is case-insensitive. Returns true or false.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} WHERE and ORDER BY were found true/false
 */
const queryContainsWhereOrderByKeywords = (fullCommandAsStringArray) => {
    const containsWhere = queryContainsWhereKeyword(fullCommandAsStringArray)
    const containsOrderBy = queryContainsOrderByKeyword(
        fullCommandAsStringArray
    )

    return containsWhere && containsOrderBy
}

/**
 * Checks whether a command contains keywords WHERE, GROUP BY and ORDER BY and that they
 * are in the correct order. Check is case-insensitive. Returns true or false.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} WHERE and ORDER BY were found true/false
 */
const queryContainsWhereGroupByOrderByKeywords = (fullCommandAsStringArray) => {
    const containsWhere = queryContainsWhereKeyword(fullCommandAsStringArray)
    const containsGroupBy = queryContainsGroupByKeyword(
        fullCommandAsStringArray
    )
    const containsOrderBy = queryContainsOrderByKeyword(
        fullCommandAsStringArray
    )

    return containsWhere && containsGroupBy && containsOrderBy
}

/**
 * Checks whether a command contains the keywords WHERE, GROUP, BY, HAVING, ORDER and BY and that they
 * are in the correct order. Check is case-insensitive. Returns true or false.
 * @param {any} fullCommandAsStringArray command as string array
 * @returns {Boolean} WHERE and GROUP BY and HAVING and ORDER BY were found true/false
 */
const queryContainsWhereGroupByHavingOrderByKeywords = (
    fullCommandAsStringArray
) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )

    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    const indexOfHaving = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'HAVING'
    )

    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    if (
        indexOfWhere < 0 ||
        indexOfGroup < 0 ||
        indexOfHaving < 0 ||
        indexOfOrder < 0
    )
        return false

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
        indexOfGroup < indexOfHaving &&
        indexOfHaving < indexOfOrder &&
        hasGroupBy &&
        hasOrderBy
    )
}

/**
 * Checks whether a command contains the keyword LIMIT and in the correct
 * position. Check is case-insensitive. Returns true or false.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} LIMIT was found true/false
 */
const queryContainsLimitKeyword = (fullCommandAsStringArray) => {
    const indexOfLimit = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'LIMIT'
    )

    if (indexOfLimit !== -1) checkLimitPosition(fullCommandAsStringArray)

    return indexOfLimit !== -1
}

module.exports = {
    queryContainsWhereKeyword,
    queryContainsGroupByKeyword,
    queryContainsGroupByHavingKeywords,
    queryContainsOrderByKeyword,
    queryContainsWhereGroupByKeywords,
    queryContainsWhereGroupByHavingKeywords,
    queryContainsWhereOrderByKeywords,
    queryContainsGroupByOrderByKeywords,
    queryContainsGroupByHavingOrderByKeywords,
    queryContainsWhereGroupByOrderByKeywords,
    queryContainsWhereGroupByHavingOrderByKeywords,
    queryContainsLimitKeyword,
}
