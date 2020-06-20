const { parseConditions } = require('./fieldParser')

/**
 * Parses the HAVING part of a command into a HAVING object from the given string array.
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

    return parsedHavingPart
}

module.exports = { parseHaving }
