const CreateTableCommand = require('./createTableCommand')
const InsertIntoCommand = require('./insertIntoCommand')
const SelectAllCommand = require('./selectAllCommand')

const commands = [CreateTableCommand, InsertIntoCommand, SelectAllCommand]

module.exports = commands
