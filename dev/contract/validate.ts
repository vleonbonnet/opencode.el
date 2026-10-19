// Validate recorded opencode.el requests against an OpenCode OpenAPI document.
// Usage: NODE_PATH=DIR-HOLDING-AJV bun validate.ts OPENAPI.json REQUESTS.jsonl
// Each request must match a documented method and path; its query
// parameters and JSON body must satisfy the documented schemas.  Exits
// non-zero when any does not.  `make contract' runs it with the ajv an
// OpenCode checkout installs.
import Ajv2020 from "ajv/dist/2020"

const [specPath, requestsPath] = process.argv.slice(2)
const spec = await Bun.file(specPath).json()
const ajv = new Ajv2020({ strict: false, allErrors: true, coerceTypes: false })
ajv.addSchema({ $id: "spec", components: spec.components }, "spec")

const rewrite = (schema: unknown): unknown => {
  if (Array.isArray(schema)) return schema.map(rewrite)
  if (schema && typeof schema === "object") {
    const out: Record<string, unknown> = {}
    for (const [key, value] of Object.entries(schema)) {
      out[key] = key === "$ref" && typeof value === "string" && value.startsWith("#/") ? `spec${value}` : rewrite(value)
    }
    return out
  }
  return schema
}

type Operation = { parameters?: any[]; requestBody?: any; operationId?: string }
const routes: { method: string; regex: RegExp; template: string; op: Operation }[] = []
for (const [template, item] of Object.entries<any>(spec.paths)) {
  const regex = new RegExp("^" + template.replace(/[.*+?^$()|[\]\\]/g, "\\$&").replace(/\{[^}]+\}/g, "[^/]+") + "$")
  for (const method of ["get", "post", "put", "patch", "delete"]) {
    if (item[method]) routes.push({ method: method.toUpperCase(), regex, template, op: item[method] })
  }
}

const validators = new Map<string, ReturnType<typeof ajv.compile>>()
const validator = (key: string, schema: unknown) => {
  if (!validators.has(key)) validators.set(key, ajv.compile(rewrite(schema) as object))
  return validators.get(key)!
}

// Query values arrive as strings: decode the way an HTTP server would.
const decodeQuery = (value: string, schema: any): unknown => {
  const types = new Set<string>()
  const collect = (s: any) => {
    if (!s) return
    if (s.$ref) {
      const name = s.$ref.split("/").pop()
      collect(spec.components.schemas[name])
    }
    if (typeof s.type === "string") types.add(s.type)
    if (Array.isArray(s.type)) s.type.forEach((t: string) => types.add(t))
    for (const k of ["anyOf", "oneOf", "allOf"]) (s[k] ?? []).forEach(collect)
  }
  collect(schema)
  if (types.has("string")) return value
  if ((types.has("number") || types.has("integer")) && /^-?\d+(\.\d+)?$/.test(value)) return Number(value)
  if (types.has("boolean") && (value === "true" || value === "false")) return value === "true"
  if (types.has("null") && value === "null") return null
  return value
}

const lines = (await Bun.file(requestsPath).text()).split("\n").filter(Boolean)
const problems = new Map<string, Set<string>>()
const seen = new Set<string>()
const note = (key: string, problem: string, test: string) => {
  if (!problems.has(key)) problems.set(key, new Set())
  problems.get(key)!.add(`${problem}   [${test}]`)
}
for (const line of lines) {
  const request = JSON.parse(line)
  const path = request.path.split("?")[0]
  const route = routes.find((r) => r.method === request.method && r.regex.test(path))
  const key = `${request.method} ${route?.template ?? path}`
  seen.add(key)
  if (!route) {
    note(key, "no such operation in the OpenAPI document", request.test)
    continue
  }
  if (request.encodeError) note(key, `body cannot be encoded: ${request.encodeError}`, request.test)
  const params = (route.op.parameters ?? []).filter((p) => p.in === "query")
  for (const [name, raw] of Object.entries<string>(request.query ?? {})) {
    const param = params.find((p) => p.name === name)
    if (!param) {
      note(key, `undocumented query parameter ${name}=${raw}`, request.test)
      continue
    }
    const value = decodeQuery(raw, param.schema)
    const validate = validator(`${key}?${name}`, param.schema)
    if (!validate(value)) note(key, `query ${name}=${raw}: ${ajv.errorsText(validate.errors)}`, request.test)
  }
  for (const param of params.filter((p) => p.required)) {
    if (!(param.name in (request.query ?? {}))) note(key, `missing required query parameter ${param.name}`, request.test)
  }
  const bodySchema = route.op.requestBody?.content?.["application/json"]?.schema
  if (request.body !== undefined) {
    if (!bodySchema) note(key, `body sent to an operation without one: ${request.body.slice(0, 120)}`, request.test)
    else {
      const validate = validator(`${key} body`, bodySchema)
      if (!validate(JSON.parse(request.body)))
        note(key, `body ${request.body.slice(0, 160)}: ${ajv.errorsText(validate.errors)}`, request.test)
    }
  } else if (route.op.requestBody?.required) note(key, "missing required body", request.test)
}
console.log(`${lines.length} requests, ${seen.size} operations, ${problems.size} with problems`)
for (const [key, list] of problems) {
  console.log(`\n${key}`)
  for (const problem of list) console.log(`  - ${problem}`)
}
process.exit(problems.size > 0 ? 1 : 0)
