const { parseOrderByFields } = require('./fieldParser')

const parseOrderBy = (slicedCommandAsStringArray) => {
    const parsedOrderByPart =
        slicedCommandAsStringArray.slice(0, 2).join(' ').toUpperCase() ===
        'ORDER BY'
            ? {
                  keyword: slicedCommandAsStringArray
                      .slice(0, 2)
                      .join(' ')
                      .toUpperCase(),
                  fields: parseOrderByFields(
                      slicedCommandAsStringArray.slice(2)
                  ),
              }
            : null

    return parsedOrderByPart
}

module.exports = { parseOrderBy }
