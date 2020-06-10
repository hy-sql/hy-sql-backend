const parseOrderBy = (slicedCommandAsStringArray) => {
    return slicedCommandAsStringArray.slice(0, 2).join(' ').toUpperCase() ===
        'ORDER BY'
        ? {
              keyword: slicedCommandAsStringArray
                  .slice(0, 2)
                  .join(' ')
                  .toUpperCase(),
              columnName: slicedCommandAsStringArray[2],
              order: slicedCommandAsStringArray
                  .slice(3)
                  .join(' ')
                  .toUpperCase()
                  .trim(),
          }
        : null
}

module.exports = { parseOrderBy }
