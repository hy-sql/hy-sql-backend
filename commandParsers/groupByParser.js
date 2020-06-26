const { parseSelectFields } = require('./fieldsParser')

/**
 * Parses the GROUP BY part of a command into a GROUP BY object from the given string array.
 * @param {string[]} slicedCommandAsStringArray the GROUP BY part of a command as string array
 */
const parseGroupBy = (slicedCommandAsStringArray) => {
    const parsedGroupByPart =
        slicedCommandAsStringArray.slice(0, 2).join(' ').toUpperCase() ===
        'GROUP BY'
            ? {
                  keyword: slicedCommandAsStringArray
                      .slice(0, 2)
                      .join(' ')
                      .toUpperCase(),
                  fields: parseSelectFields(
                      slicedCommandAsStringArray.slice(2)
                  ),
              }
            : null

    return parsedGroupByPart
}

module.exports = { parseGroupBy }
