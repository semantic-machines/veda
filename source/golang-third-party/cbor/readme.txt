этот файл был взят из проекта bitbucket.org/bodhisnarkva/cbor/go
в нем были произведены изменения в Decoder связанные с порядком байтов в декодировании числа:

func (dec *Decoder) handleInfoBits(cborInfo byte) (uint64, error) {
        var aux uint64
        
        if (cborInfo <= 23) {
                aux = uint64(cborInfo)
                return aux, nil
        } else if (cborInfo == int8Follows) {
                didread, err := io.ReadFull(dec.rin, dec.b8[:1])
                if didread == 1 {
                        aux = uint64(dec.b8[0])
                }
                return aux, err
        } else if (cborInfo == int16Follows) {
                didread, err := io.ReadFull(dec.rin, dec.b8[:2])
                if didread == 2 {
                        aux = (uint64(dec.b8[1]) << 8) | uint64(dec.b8[0])
                }
                return aux, err
        } else if (cborInfo == int32Follows) {
                didread, err := io.ReadFull(dec.rin, dec.b8[:4])
                if didread == 4 {
                aux = (uint64(dec.b8[3]) << 24) |
                        (uint64(dec.b8[2]) << 16) |
                        (uint64(dec.b8[1]) <<  8) |
                        uint64(dec.b8[0])
                }
                return aux, err
        } else if (cborInfo == int64Follows) {
                didread, err := io.ReadFull(dec.rin, dec.b8)
                if didread == 8 {
                        var shift uint = 0
                        i := 0
                        aux = uint64(dec.b8[i]) << shift
                        for i < 7 {
                                i += 1
                                shift += 8
                                aux |= uint64(dec.b8[i]) << shift
                        }
                }
                return aux, err
        }
        return 0, nil
}

