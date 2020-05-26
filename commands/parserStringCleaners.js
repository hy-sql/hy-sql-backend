const cleanStringArray = (columnsAsStringList) => {
    return columnsAsStringList
        .join(' ')
        .split(', ')
        .map((col) => col.trim())
}

const addAttributesToValuesArray = (columns, stringArray) => {
    const taulukko = stringArray.map((value, index) =>
        value.match('[0-9]')
            ? {
                column: columns[index] ? columns[index].name : null,
                value,
                type: 'INTEGER',
            }
            : {
                column: columns[index] ? columns[index].name : null,
                value: value.replace(/'/g, ' ').trim(),
                type: 'TEXT',
            }
    )
    return taulukko
}

module.exports = { cleanStringArray, addAttributesToValuesArray }
