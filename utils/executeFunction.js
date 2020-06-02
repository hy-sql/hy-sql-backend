const executeFunction = (functionField, columnValue) => {
    switch (functionField.name) {
        case 'LENGTH':
            return columnValue.length
    }
}

module.exports = { executeFunction }
