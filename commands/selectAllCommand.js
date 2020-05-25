const selectAllSchema = require('../models/SelectAllSchema')

const parseCommand = (fullCommandAsStringList) => {
    const parsedCommand = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        from: fullCommandAsStringList[2],
        tableName: fullCommandAsStringList[3],
        finalSemicolon: fullCommandAsStringList[4],
    }

    return selectAllSchema.validate(parsedCommand)
}

module.exports = { parseCommand }
