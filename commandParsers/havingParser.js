const { parseConditions } = require('./conditionsParser')
const SQLError = require('../models/SQLError')

/**
 * Parses the HAVING part of a command into a HAVING object from the given string array.
 * This parser is copy of whereParser.
 * @param {string[]} slicedCommandAsStringArray the HAVING part of a command as string array
 */
const parseHaving = (slicedCommandAsStringArray) => {
    const indexOfOrder = slicedCommandAsStringArray.findIndex(
        (c) => c.toUpperCase() === 'ORDER'
    )

    const parsedHavingPart = {
        keyword: slicedCommandAsStringArray[0],
        conditions: parseConditions(
            slicedCommandAsStringArray.slice(
                1,
                indexOfOrder >= 0
                    ? indexOfOrder
                    : slicedCommandAsStringArray.length
            )
        ),
    }

    if (
        parsedHavingPart.conditions.AND.length === 0 &&
        parsedHavingPart.conditions.OR.length === 0
    ) {
        throw new SQLError('HAVING clause must contain at least one condition')
    }

    return parsedHavingPart
}

module.exports = { parseHaving }
