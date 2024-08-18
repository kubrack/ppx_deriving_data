# Description
ppx deriver that will generate random data from the record type definition

# Usage

add to dune:
```
(libraries ppx_deriving_data)
(preprocess (pps ppx_deriving_data))
```

dune utop:
```
utop # type block = {
  id: int;
  nums: int list;
  strs: (int * string) list
} [@@deriving random_data]

type owner = {
  id: int;
  name: string;
} [@@deriving random_data]

type changes = {
  blocks: block list;
  owners: owner list;
} [@@deriving random_data];;
type block = { id : int; nums : int list; strs : (int * string) list; }
val random_block : unit -> block = <fun>
type owner = { id : int; name : string; }
val random_owner : unit -> owner = <fun>
type changes = { blocks : block list; owners : owner list; }
val random_changes : unit -> changes = <fun>

utop # random_changes();;
- : changes =
{blocks =
  [{id = 34; nums = [36; 26; 4];
    strs =
     [(74, "ptwedwf"); (38, "uedvlbzfl"); (70, "fqrjeyg"); (88, "jzb");
      (95, "exzfb")]};
   {id = 90; nums = [96; 37; 0; 75];
    strs =
     [(59, "czeoahjv"); (7, "h"); (53, "d"); (4, "zhbuzjyyw");
      (44, "ijnwnvcpb")]};
   {id = 66; nums = [74; 62; 87]; strs = [(91, "xh")]};
   {id = 71; nums = [94; 87; 4; 12; 28];
    strs = [(10, "zyri"); (49, "slobcrorn"); (80, "lhno"); (66, "zgqnhyue")]};
   {id = 76; nums = [98; 15]; strs = [(90, "ydnmlb")]}];
 owners = [{id = 56; name = "copjaocb"}; {id = 16; name = "og"}]}

```