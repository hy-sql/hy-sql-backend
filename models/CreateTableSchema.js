const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    name: Joi.string().alphanum().max(64).required(),

    type: Joi.string().valid('INTEGER', 'TEXT'),

    primaryKey: Joi.boolean(),
})

const CreateTableSchema = Joi.object({
    name: Joi.string().valid('CREATE TABLE').insensitive().required().messages({
        'any.only': 'Command must be "CREATE TABLE"',
    }),

    tableName: Joi.string().alphanum().max(64).required().messages({
        'string.base': 'this is not a string',
    }),

    openingBracket: Joi.string().valid('(').required(),

    columns: Joi.array().items(ColumnsSchema),

    closingBracket: Joi.string().valid(')').required(),

    finalSemicolon: Joi.string().valid(';').required(),
})

module.exports = { CreateTableSchema, ColumnsSchema }
