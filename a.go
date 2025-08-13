func main() int32 {
    type one.1 struct{
        field1 int32
        field2 float64
        nested two.2
        field4 rune
    }
    var x.1 one.1 = {1 /* int32 */ /* int32 */, float64(1) /* float64 */ /* float64 */, {{1 /* int32 */ /* int32 */} /* three.3 */} /* two.2 */, rune(97 /* int32 */) /* rune */ /* rune */} /* one.1 */
}
