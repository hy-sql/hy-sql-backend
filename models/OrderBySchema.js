const Joi = require('@hapi/joi')

const OrderBySchema = Joi.object({
    keyword: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^ORDER BY$/)
        .insensitive()
        .optional()
        .messages({
            'any.required':
                'This query is expected to contain the following keyword: ORDER BY',
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base':
                'ORDER BY is either misspelled, missing or in the wrong position',
        }),

    columnName: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^ASC$|^DESC$/i, { name: 'keywordCheck', invert: true })
        .pattern(/^\w+$/)
        .max(64)
        .required()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'string.pattern.base':
                'Only letters, numbers and underscore allowed in a column name',
            'string.max': 'A column name is too long',
            'any.required': 'A column name is missing',
            'string.pattern.invert.name':
                'Keywords ASC and DESC must appear after column name',
        }),

    order: Joi.string()
        .pattern(/[;]/, { invert: true })
        .pattern(/^ASC.+$|^DESC.+$/i, { name: 'extra', invert: true })
        .pattern(/^ASC$|^DESC$/i)
        .allow('')
        .insensitive()
        .max(64)
        .optional()
        .messages({
            'string.pattern.invert.base':
                'Semicolon should be only found at the end of a query',
            'string.pattern.base':
                'Not a sorting keyword at the end of the query',
            'string.pattern.invert.name':
                'Extra characters at the end of the line are causing command to fail',
        }),
})

module.exports = OrderBySchema
