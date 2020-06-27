const Joi = require('@hapi/joi')
const WhereSchema = require('./WhereSchema')
const GroupBySchema = require('./GroupBySchema')
const HavingSchema = require('./HavingSchema')
const OrderBySchema = require('./OrderBySchema')
const {
    AllFieldsSchema,
    ColumnSchema,
    TextSchema,
    IntegerSchema,
} = require('./FieldSchemas')
const { ExpressionSchema } = require('./ExpressionSchema')
const DistinctSchema = require('./DistinctSchema')
const FunctionSchema = require('./FunctionSchema')
const { LimitSchema } = require('./LimitSchema')
const SQLError = require('../models/SQLError')

/**
 * Joi schema for validating SELECT commands not containing WHERE or ORDER BY.
 */
const SelectSchema = Joi.object({
    name: Joi.string().required().valid('SELECT').insensitive().messages({
        'any.only': 'Query must begin with SELECT *',
        'any.required': 'Query must begin with SELECT *',
    }),

    fields: Joi.array()
        .items(
            AllFieldsSchema,
            ColumnSchema,
            TextSchema,
            IntegerSchema,
            ExpressionSchema.shared(FunctionSchema),
            FunctionSchema.shared(ExpressionSchema),
            DistinctSchema
        )
        .min(1)
        .required(),

    from: Joi.string()
        .required()
        .pattern(/;/, { invert: true })
        .pattern(/^FROM$/i)
        .messages({
            'string.pattern.invert.base':
                'Semicolon should only be found at the end of a query',
            'string.pattern.base': 'SELECT * must be followed by FROM',
            'any.required': 'SELECT * must be followed by FROM',
        }),

    tableName: Joi.string().required().pattern(/^\w+$/).max(64).messages({
        'any.required': 'Query must contain a table name',
        'string.pattern.base':
            'Table name should only contain one or more alphanumeric characters and underscores',
        'string.max': 'The table name is too long',
    }),

    finalSemicolon: Joi.string().required().valid(';').messages({
        'any.only': 'Query must end with ;',
        'any.required': 'Query must end with ;',
    }),

    limit: LimitSchema,

    unrecognized: Joi.array()
        .max(0)
        .error((errors) => {
            throw new SQLError(
                `The following part of the query is probably incorrect and causing it to fail: ${errors[0].value.join(
                    ' '
                )}`
            )
        }),
})

/**
 * Joi schema for validating SELECT commands containing WHERE.
 */
const SelectWhereSchema = SelectSchema.keys({
    where: WhereSchema,
})

/**
 * Joi schema for validating SELECT commands containing GROUP BY.
 */
const SelectGroupBySchema = SelectSchema.keys({
    groupBy: GroupBySchema,
})

/**
 * Joi schema for validating SELECT commands containing GROUP BY and HAVING.
 */
const SelectGroupByHavingSchema = SelectSchema.keys({
    groupBy: GroupBySchema,
    having: HavingSchema,
})

/**
 * Joi schema for validating SELECT commands containing ORDER BY.
 */
const SelectOrderBySchema = SelectSchema.keys({
    orderBy: OrderBySchema,
})

/**
 * Joi schema for validating SELECT commands containing WHERE and GROUP BY.
 */
const SelectWhereGroupBySchema = SelectSchema.keys({
    where: WhereSchema,
    groupBy: GroupBySchema,
})

/**
 * Joi schema for validating SELECT commands containing WHERE, GROUP BY and HAVING.
 */
const SelectWhereGroupByHavingSchema = SelectSchema.keys({
    where: WhereSchema,
    groupBy: GroupBySchema,
    having: HavingSchema,
})

/**
 * Joi schema for validating SELECT commands containing WHERE and ORDER BY.
 */
const SelectWhereOrderBySchema = SelectSchema.keys({
    where: WhereSchema,
    orderBy: OrderBySchema,
})

/**
 * Joi schema for validating SELECT commands containing GROUP BY and ORDER BY.
 */
const SelectGroupByOrderBySchema = SelectSchema.keys({
    groupBy: GroupBySchema,
    orderBy: OrderBySchema,
})

/**
 * Joi schema for validating SELECT commands containing GROUP BY, HAVING and ORDER BY.
 */
const SelectGroupByHavingOrderBySchema = SelectSchema.keys({
    groupBy: GroupBySchema,
    having: HavingSchema,
    orderBy: OrderBySchema,
})

/**
 * Joi schema for validating SELECT commands containing WHERE, GROUP BY and ORDER BY.
 */
const SelectWhereGroupByOrderBySchema = SelectSchema.keys({
    where: WhereSchema,
    groupBy: GroupBySchema,
    orderBy: OrderBySchema,
})

/**
 * Joi schema for validating SELECT commands containing WHERE, GROUP BY, HAVING and
 * ORDER BY.
 */
const SelectWhereGroupByHavingOrderBySchema = SelectSchema.keys({
    where: WhereSchema,
    groupBy: GroupBySchema,
    having: HavingSchema,
    orderBy: OrderBySchema,
})

module.exports = {
    SelectSchema,
    SelectWhereSchema,
    SelectGroupBySchema,
    SelectGroupByHavingSchema,
    SelectOrderBySchema,
    SelectWhereGroupBySchema,
    SelectWhereGroupByHavingSchema,
    SelectWhereOrderBySchema,
    SelectGroupByOrderBySchema,
    SelectGroupByHavingOrderBySchema,
    SelectWhereGroupByOrderBySchema,
    SelectWhereGroupByHavingOrderBySchema,
}
