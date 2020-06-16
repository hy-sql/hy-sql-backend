const { parseOrderByFields } = require('./fieldParser')

/**
 * Parses the ORDER BY part of a command into an object from the given string array.
 * @param {string[]} slicedCommandAsStringArray the ORDER BY part of a command as string array
 */
const parseOrderBy = (slicedCommandAsStringArray) => {
    const parsedOrderByPart =
        slicedCommandAsStringArray.slice(0, 2).join(' ').toUpperCase() ===
        'ORDER BY'
            ? {
                  keyword: slicedCommandAsStringArray
                      .slice(0, 2)
                      .join(' ')
                      .toUpperCase(),
                  columns: parseOrderByFields(
                      slicedCommandAsStringArray.slice(2)
                  ),
              }
            : null

    return parsedOrderByPart
}

module.exports = { parseOrderBy }
