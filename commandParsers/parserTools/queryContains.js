/** Checks whether a command contains the keyword WHERE. Check is
 * case-insensitive. Returns true or false.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} WHERE was found true/false
 */
const queryContainsWhereKeyword = (fullCommandAsStringArray) => {
    const where = fullCommandAsStringArray.findIndex(
        (string) => string.toUpperCase() === 'WHERE'
    )
    return where !== -1
}

/** Checks whether a command contains the keywords ORDER and BY and that they
 * are in the correct order. Check is case-insensitive. Returns true or false.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} ORDER BY was found true/false
 */
const queryContainsOrderByKeywords = (fullCommandAsStringArray) => {
    const hasOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    const hasBy = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'BY'
    )

    return hasOrder > 0 && hasBy > 0 ? hasOrder < hasBy : false
}

/** Checks whether a command contains the keywords ORDER, BY and WHERE and that they
 * are in the correct order. Check is case-insensitive. Returns true or false.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @returns {Boolean} ORDER BY and WHERE were found true/false
 */
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
