const { parseSelectFields } = require('./fieldParser')

const parseGroupBy = (slicedCommandAsStringArray) => {
    const parsedGroupByPart =
        slicedCommandAsStringArray.slice(0, 2).join(' ').toUpperCase() ===
        'GROUP BY'
            ? {
                  keyword: slicedCommandAsStringArray
                      .slice(0, 2)
                      .join(' ')
                      .toUpperCase(),
                  columns: parseSelectFields(
                      slicedCommandAsStringArray.slice(2)
                  ),
              }
            : null

    return parsedGroupByPart
}

module.exports = { parseGroupBy }
