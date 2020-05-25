const Joi = require('@hapi/joi')

const ColumnsSchema = Joi.object({
    name: Joi.string()
        .pattern(/[;]./, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .required()
        .messages({
            'string.pattern.invert.base': 'Semicolon not allowed here',
        }),

    type: Joi.string()
        .pattern(/[;]./, { invert: true })
        .valid('INTEGER', 'TEXT')
        .insensitive()
        .required()
        .messages({
            'string.pattern.invert.base': 'Semicolon not allowed here',
        }),

    constraints: Joi.array()
        .items(
            Joi.string()
                .pattern(/[;]/, { invert: true })
                .pattern(/^PRIMARY KEY$/)
                .insensitive()
                .optional()
                .messages({
                    'string.pattern.invert.base': 'Semicolon not allowed here',
                })
        )
        .messages({
            'string.pattern.invert.base': 'Semicolon not allowed here',
        }),
})

const CreateTableSchema = Joi.object({
    name: Joi.string()
        .pattern(/[;]./, { invert: true })
        .valid('CREATE TABLE')
        .insensitive()
        .required()
        .messages({
            'string.pattern.invert.base': 'Semicolon not allowed here',
        }),

    tableName: Joi.string()
        .pattern(/[;]./, { invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .required()
        .messages({
            'string.pattern.invert.base': 'Semicolon not allowed here',
        }),

    openingBracket: Joi.boolean().valid(true).required().messages({}),

    columns: Joi.array().items(ColumnsSchema).required(),

    closingBracket: Joi.boolean().valid(true).required().messages({}),

    finalSemicolon: Joi.string().valid(';').optional().messages({}),
})

module.exports = { CreateTableSchema, ColumnsSchema }
