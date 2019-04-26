## TODO

TS annotations:

```javascript
declare class C {
  a(): void;
  b(): void /* ts:undefined */;
}
```

Output:

```typescript
declare class C {
  a(): void
  b(): undefined
}
```
