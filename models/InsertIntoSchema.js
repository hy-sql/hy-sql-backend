const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    name: Joi.string().pattern(/^\w+$/).max(64).required().messages({}),
})

const InsertIntoSchema = Joi.object({
    name: Joi.string()
        .min(3)
        .max(30)
        .valid('INSERT INTO')
        .insensitive()
        .required()
        .messages({
            'any.only': 'Command must be "INSERT INTO"',
        }),

    tableName: Joi.string().alphanum().min(2).max(64).required().messages({
        'string.base': 'this is not a string',
    }),

    columnsOpeningBracket: Joi.string().valid('(').required(),

    columns: Joi.array().min(1).items(ColumnsSchema).required().messages({
        'array.base': 'this is not an array',
        'array.min': 'there should be at least one column',
    }),

    columnsClosingBracket: Joi.string().valid(')').required(),

    anchorKeyword: Joi.string().valid('VALUES').required(),

    valuesOpeningBracket: Joi.string().valid('(').required(),

    values: Joi.array().min(1).required().messages({
        'array.base': 'this is not an array',
        'array.min': 'there should be at least one value',
    }),

    valuesClosingBracket: Joi.string().valid(')').required(),

    finalSemicolon: Joi.string().valid(';').required(),
})

module.exports = { InsertIntoSchema }
