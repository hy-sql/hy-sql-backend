const SQLError = require('../../models/SQLError')

/**
 * Checks whether the LIMIT keyword is correctly positioned after keywords WHERE,
 * GROUP BY, HAVING and ORDER BY. Also checks that the keyword OFFSET is not
 * found before keyword LIMIT. Throws an error if not correctly positioned.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const checkLimitPosition = (fullCommandAsStringArray) => {
    const indexOfLimit = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'LIMIT'
    )

    const indexOfOffset = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'OFFSET'
    )
    if (indexOfOffset !== -1 && indexOfOffset < indexOfLimit) {
        throw new SQLError('OFFSET must always be positioned after LIMIT')
    }

    if (
        !afterWhere(fullCommandAsStringArray, indexOfLimit) ||
        !afterGroupBy(fullCommandAsStringArray, indexOfLimit) ||
        !afterHaving(fullCommandAsStringArray, indexOfLimit) ||
        !afterOrderBy(fullCommandAsStringArray, indexOfLimit)
    ) {
        throw new SQLError(
            'LIMIT must always be positioned after WHERE, GROUP BY, HAVING and ORDER BY'
        )
    }
}

/**
 * Checks whether the WHERE keyword is correctly positioned before keywords
 * GROUP BY, HAVING and ORDER BY. Throws an error if not correctly positioned.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const checkWherePosition = (fullCommandAsStringArray) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )

    if (
        !beforeGroupBy(fullCommandAsStringArray, indexOfWhere) ||
        !beforeHaving(fullCommandAsStringArray, indexOfWhere) ||
        !beforeOrderBy(fullCommandAsStringArray, indexOfWhere)
    ) {
        throw new SQLError(
            'WHERE must always be positioned before GROUP BY, HAVING and ORDER BY'
        )
    }
}

/**
 * Checks whether the GROUP BY keyword is correctly positioned after keyword
 * WHERE and before HAVING and ORDER BY. Throws an error if not correctly
 * positioned.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const checkGroupByPosition = (fullCommandAsStringArray) => {
    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )
    if (
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() !== 'BY'
    ) {
        return
    }

    if (
        !afterWhere(fullCommandAsStringArray, indexOfGroup) ||
        !beforeHaving(fullCommandAsStringArray, indexOfGroup) ||
        !beforeOrderBy(fullCommandAsStringArray, indexOfGroup)
    ) {
        throw new SQLError(
            'GROUP BY must always be positioned after WHERE and before HAVING and ORDER BY'
        )
    }
}

/**
 * Checks whether the HAVING keyword is correctly positioned after keywords
 * WHERE and GROUP BY and before ORDER BY. Throws an error if not correctly
 * positioned.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const checkHavingPosition = (fullCommandAsStringArray) => {
    const indexOfHaving = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'HAVING'
    )

    if (
        !afterWhere(fullCommandAsStringArray, indexOfHaving) ||
        !afterGroupBy(fullCommandAsStringArray, indexOfHaving) ||
        !beforeOrderBy(fullCommandAsStringArray, indexOfHaving)
    ) {
        throw new SQLError(
            'HAVING must always be positioned after WHERE and GROUP BY and before ORDER BY'
        )
    }
}

/**
 * Checks whether the ORDER BY keyword is correctly positioned after keywords
 * WHERE, GROUP BY and HAVING. Throws an error if not correctly positioned.
 * @param {string[]} fullCommandAsStringArray command as string array
 */
const checkOrderByPosition = (fullCommandAsStringArray) => {
    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )
    if (
        fullCommandAsStringArray[indexOfOrder + 1] &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() !== 'BY'
    ) {
        return
    }

    if (
        !afterWhere(fullCommandAsStringArray, indexOfOrder) ||
        !afterGroupBy(fullCommandAsStringArray, indexOfOrder) ||
        !afterHaving(fullCommandAsStringArray, indexOfOrder)
    ) {
        throw new SQLError(
            'ORDER BY must always be positioned after WHERE, GROUP BY and HAVING'
        )
    }
}

/**
 * Checks that a keyword is after WHERE in the given command. Returns
 * true or false. Returns true if command does not contain keyword WHERE.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @param {Number} indexOfKeyword index of keyword
 * @return {Boolean} keyword is after WHERE true/false
 */
const afterWhere = (fullCommandAsStringArray, indexOfKeyword) => {
    const indexOfWhere = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'WHERE'
    )

    return indexOfKeyword > indexOfWhere
}

/**
 * Checks that a keyword is before GROUP BY in the given command. Returns
 * true or false. Returns true if command does not contain keyword GROUP BY.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @param {Number} indexOfKeyword index of keyword
 * @return {Boolean} keyword is before GROUP BY true/false
 */
const beforeGroupBy = (fullCommandAsStringArray, indexOfKeyword) => {
    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    return indexOfGroup !== -1 &&
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY' &&
        indexOfKeyword > indexOfGroup
        ? false
        : true
}

/**
 * Checks that a keyword is after GROUP BY in the given command. Returns
 * true or false. Returns true if command does not contain keyword GROUP BY.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @param {Number} indexOfKeyword index of keyword
 * @return {Boolean} keyword is after GROUP BY true/false
 */
const afterGroupBy = (fullCommandAsStringArray, indexOfKeyword) => {
    const indexOfGroup = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'GROUP'
    )

    return indexOfGroup !== -1 &&
        fullCommandAsStringArray[indexOfGroup + 1] &&
        fullCommandAsStringArray[indexOfGroup + 1].toUpperCase() === 'BY' &&
        indexOfKeyword < indexOfGroup
        ? false
        : true
}

/**
 * Checks that a keyword is before HAVING in the given command. Returns
 * true or false. Returns true if command does not contain keyword HAVING.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @param {Number} indexOfKeyword index of keyword
 * @return {Boolean} keyword is before HAVING true/false
 */
const beforeHaving = (fullCommandAsStringArray, indexOfKeyword) => {
    const indexOfHaving = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'HAVING'
    )

    return !(indexOfHaving !== -1 && indexOfKeyword > indexOfHaving)
}

/**
 * Checks that a keyword is after HAVING in the given command. Returns
 * true or false. Returns true if command does not contain keyword HAVING.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @param {Number} indexOfKeyword index of keyword
 * @return {Boolean} keyword is after HAVING true/false
 */
const afterHaving = (fullCommandAsStringArray, indexOfKeyword) => {
    const indexOfHaving = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'HAVING'
    )

    return indexOfKeyword > indexOfHaving
}

/**
 * Checks that a keyword is before ORDER BY in the given command. Returns
 * true or false. Returns true if command does not contain keyword ORDER BY.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @param {Number} indexOfKeyword index of keyword
 * @return {Boolean} keyword is before ORDER BY true/false
 */
const beforeOrderBy = (fullCommandAsStringArray, indexOfKeyword) => {
    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    return indexOfOrder !== -1 &&
        fullCommandAsStringArray[indexOfOrder + 1] &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY' &&
        indexOfKeyword > indexOfOrder
        ? false
        : true
}

/**
 * Checks that a keyword is after ORDER BY in the given command. Returns
 * true or false. Returns true if command does not contain keyword ORDER BY.
 * @param {string[]} fullCommandAsStringArray command as string array
 * @param {Number} indexOfKeyword index of keyword
 * @return {Boolean} keyword is after ORDER BY true/false
 */
const afterOrderBy = (fullCommandAsStringArray, indexOfKeyword) => {
    const indexOfOrder = fullCommandAsStringArray.findIndex(
        (s) => s.toUpperCase() === 'ORDER'
    )

    return indexOfOrder !== -1 &&
        fullCommandAsStringArray[indexOfOrder + 1] &&
        fullCommandAsStringArray[indexOfOrder + 1].toUpperCase() === 'BY' &&
        indexOfKeyword < indexOfOrder
        ? false
        : true
}

module.exports = {
    checkLimitPosition,
    checkGroupByPosition,
    checkOrderByPosition,
    checkWherePosition,
    checkHavingPosition,
}
