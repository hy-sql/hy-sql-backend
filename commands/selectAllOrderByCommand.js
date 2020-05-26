const selectAllSchema = require('../models/SelectAllSchema')

const parseCommand = (fullCommandAsStringList) => {
    const parsedCommand = {
        name: fullCommandAsStringList.slice(0, 2).join(' '),
        from: fullCommandAsStringList[2],
        tableName: fullCommandAsStringList[3],
        orderBy: fullCommandAsStringList.slice(
            4,
            fullCommandAsStringList.length - 1
        ),
        finalSemicolon:
            fullCommandAsStringList[fullCommandAsStringList.length - 1],
    }

    console.log(parsedCommand)

    return selectAllSchema.validate(parsedCommand)
}

module.exports = { parseCommand }
