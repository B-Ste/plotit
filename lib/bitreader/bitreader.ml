type bitreader = {mutable data: int list; mutable pos: int}

        exception Empty_bitreader
        
        (** Creates and returns a new bitreader. Note that the given list is interpretet as bytes, 
            which means, that only the least significant 8 bits are considered. *)
        let create_bitreader l = {data = l; pos = 0};;
        
        (** Reads Bit from bitreader and outputs it. Raises [Empty_bitreader] iff no bit can be read. *)
        let read_bit br =
            match br.data with
            | [] -> raise Empty_bitreader
            | h :: t -> let i = Int.logand h 1 in
                if br.pos = 7 then
                    (br.data <- t;
                    br.pos <- 0; 
                    i)
                else
                    (br.data <- Int.shift_right_logical h 1 :: t;
                    br.pos <- br.pos + 1;
                    i);;

        (** Reads a entire byte from the bitreader an outputs it. If the bitreader is not aligned on a byte, all bits to
            the next byte-aligned position are ignored and the following byte iutputted. Raises [Empty_bitreader] if no
            byte can be read. *)
        let rec read_byte br = 
            match br.data with
            | [] -> raise Empty_bitreader
            | h :: t -> 
                if br.pos = 0 then
                    (br.data <- t;
                    h)
                else 
                    (br.data <- t;
                    br.pos <- 0;
                    read_byte br);;

        let rec range a b = if a = b then [] else a :: range (a + 1) b
        
        (** [read_bits br n] reads [n] bits from bitreader [br] and combines them into one [int]. The bit read first is the
            LSB and the bit read last is the MSB. Raises [Empty_bitreader] if not all bits could be read. *)
        let read_bits br n = List.fold_left (fun a i -> Int.logor (Int.shift_left (read_bit br) i) a) 0 (range 0 n)
        
        (** [read_bytes br n] reads [n] bytes from bitreader [br] and combines the into one [int]. The bytes are ordered in 
            little-endian order. Raises [Empty_bitreader] if not all bits could be read. *)
        let read_bytes br n = List.fold_left (fun a i -> Int.logor (Int.shift_left (read_byte br) (i * 8)) a) 0 (range 0 n)
        