defmodule Khf1 do

  @moduledoc """
  Számtekercs-leíró konvertálása és ellenőrzése
  @author "Koczó Attila <koczoa@edu.bme.hu>"
  @date   "2024-09-16"
  """
    
  # Alapadatok
  @type size()  :: integer() # tábla mérete (0 < n)
  @type cycle() :: integer() # ciklus hossza (0 < m <= n)
  @type value() :: integer() # mező értéke (0 < v <= m)
    
  # Mezőkoordináták
  @type row()   :: integer()       # sor száma (1-től n-ig)
  @type col()   :: integer()       # oszlop száma (1-től n-ig)
  @type field() :: {row(), col()}  # mező koordinátái
    
  # Feladványleírók
  @type field_value() :: {field(), value()}                 # mező és értéke
  @type puzzle_desc() :: {size(), cycle(), [field_value()]} # feladvány
  @type list_desc() :: [String.t()] # 1. elem: méret, 2. elem: ciklushossz,
                                    # többi elem esetleg: mezők és értékük

  
  @spec to_internal(ps::list_desc()) :: pd::puzzle_desc()
  # A ps szöveges feladványleíró-listának megfelelő adatstruktúra pd
  def to_internal([size, cycle | values]) do 
    {
      String.to_integer(String.trim(size)),
      String.to_integer(String.trim(cycle)),
      values_to_enum(values)
    }
  end


  @spec values_to_enum(list::[String.t()]) :: list::[field_value()]
  # A ps szöveges feladványleíró-listának megfelelő adatstruktúra pd
  defp values_to_enum([]) do
    []
  end

  defp values_to_enum([current | rest]) do
    [row, col, value] = String.split(current) |> Enum.map(&String.to_integer/1)
    [{{row, col}, value} | values_to_enum(rest)]
  end

  

  @spec correct?(pd::puzzle_desc()) :: b::boolean()
  # b igaz, ha a pd feladványleíró helyes, egyébként hamis
  
  def correct?({n, m, fields}) do 
    0 < n 
    and 0 < m 
    and m <= n
    and correct_fields(n, m, fields)
    and Enum.frequencies_by(fields,
      fn {{row, col}, _} -> {row, col} end)
      |> Map.values()
      |> Enum.all?(fn x -> x == 1 end)
  end


  @spec correct_fields(n::integer(), m::integer(), pd::puzzle_desc()) :: b::boolean()
  # b igaz, ha az alábbi feltételek teljesülnek:
  # 0 < n, 0 < m ≤ n
  # 0 < row ≤ n, 0 < col ≤ n
  # 0 < val ≤ m
  
  defp correct_fields(_, _, []) do
    true
  end

  defp correct_fields(n, m, [{{row, col}, val} | rest]) do
    0 < row
    and row <= n
    and 0 < col 
    and col <= n
    and 0 < val
    and val <= m
    and correct_fields(n, m, rest)
  end
    
end


defmodule Khf2 do
  @moduledoc """
  Számtekercs kiterítése
  @author "Koczó Attila <koczoa@edu.bme.hu>"
  @date   "2024-09-28"
  ...
  """

  # Alapadatok
  @type size()  :: integer() # tábla mérete (0 < n)
  @type cycle() :: integer() # ciklus hossza (0 < m <= n)
  @type value() :: integer() # mező értéke (0 < v <= m vagy "")

  # Mezőkoordináták
  @type row()   :: integer()       # sor száma (1-től n-ig)
  @type col()   :: integer()       # oszlop száma (1-től n-ig)
  @type field() :: {row(), col()}  # mező koordinátái

  # Feladványleírók
  @type field_value() :: {field(), value()}           # mező és értéke
  @type field_opt_value() :: {field(), value() | nil} # mező és opcionális értéke

  @type list_desc() :: [String.t()] # 1. elem: méret, 2. elem: ciklushossz,
                                    # többi elem esetleg: mezők és értékük

  # @spec helix(ps::list_desc()) :: gs::[field_opt_value()]
  # A ps szöveges feladványleíró-lista szerinti számtekercs kiterített listája gs
  def helix(list) do
    {s, _, list} = Khf1.to_internal(list)
    balra(1, 1, s, 1, Enum.into(list, %{}))
  end


  @spec balra(x::integer(), y::integer(), size::integer(), m::integer(), data::%{}) ::  gs::[field_opt_value()]
  # Az x, y koodirnátákból kiszámolja, hogy erről a mezőről a következő mező eléréséhez
  # milyen irányba kell mozogni, hogy a kakaóscsiga irányában járjuk be a mezőket, valamint
  # a data map adatstruktúrában megnézi, hogy a szöveges leíró listában szerepel-e ehhez a mezőhöz
  # tartozó érték
  def balra(x, y, size, m, data) do
    if(x != size) do 
      [{{y, x}, data[{y, x}]} | balra(x+1, y, size, m, data)]
    else
      if(size != m) do
        le(x, y, size, m, data)
      else
        [{{y, x}, data[{y, x}]}]
      end
    end
  end  


  # @spec le(x::int(), y::int(), size::int(), m::int(), data::%{}) ::  gs::[field_opt_value()]
  # Az x, y koodirnátákból kiszámolja, hogy erről a mezőről a következő mező eléréséhez
  # milyen irányba kell mozogni, hogy a kakaóscsiga irányában járjuk be a mezőket, valamint
  # a data map adatstruktúrában megnézi, hogy a szöveges leíró listában szerepel-e ehhez a mezőhöz
  # tartozó érték
  defp le(x, y, size, m, data) do
    if(y != size) do
      [{{y, x}, data[{y, x}]} | le(x, y+1, size, m, data)]
    else
      if(size != m) do
        jobbra(x, y, size, m, data)
      else
        [{{y, x}, data[{y, x}]}]
      end
    end
  end


  # @spec jobbra(x::int(), y::int(), size::int(), m::int(), data::%{}) ::  gs::[field_opt_value()]
  # Az x, y koodirnátákból kiszámolja, hogy erről a mezőről a következő mező eléréséhez
  # milyen irányba kell mozogni, hogy a kakaóscsiga irányában járjuk be a mezőket, valamint
  # a data map adatstruktúrában megnézi, hogy a szöveges leíró listában szerepel-e ehhez a mezőhöz
  # tartozó érték
  defp jobbra(x, y, size, m, data) do
    if(x != m) do 
      [{{y, x}, data[{y, x}]} | jobbra(x-1, y, size, m, data)]
    else
      if(size != m) do
        fel(x, y, size, m+1, data)
      else
        [{{y, x}, data[{y, x}]}]
      end
    end
  end


  # @spec fel(x::int(), y::int(), size::int(), m::int(), data::%{}) ::  gs::[field_opt_value()]
  # Az x, y koodirnátákból kiszámolja, hogy erről a mezőről a következő mező eléréséhez
  # milyen irányba kell mozogni, hogy a kakaóscsiga irányában járjuk be a mezőket, valamint
  # a data map adatstruktúrában megnézi, hogy a szöveges leíró listában szerepel-e ehhez a mezőhöz
  # tartozó érték
  defp fel(x, y, size, m, data) do
    if(y != m) do
      [{{y, x}, data[{y, x}]} | fel(x, y-1, size, m, data)]
    else
      if(size != m) do
        balra(x, y, size-1, m, data)
      else
        [{{y, x}, data[{y, x}]}]
      end
    end
  end
end

defmodule Khf3 do
  @moduledoc """
  Ciklikus számlisták
  @author "Koczó Attila <koczoa@edu.bme.hu>"
  @date   "2024-10-06"
  ...
  """
  @type count() :: integer() # számsorozatok száma, n (1 < n)
  @type cycle() :: integer() # számsorozat hossza, m (1 <= m)
  @type size()  :: integer() # listahossz, len (1 < len)
  @type value() :: integer() # listaelem értéke, val (0 <= val <= m)
  @type index() :: integer() # listaelem sorszáma, ix (1 <= ix <= len)
  @type index_value() :: {index(), value()} # listaelem indexe és értéke

  @spec cyclists({n::count(), m::cycle(), len::size()}, constraints::[index_value()]) \
    :: results::[[value()]]
  # results az összes olyan len hosszú lista listája, melyekben
  # * az 1-től m-ig tartó számsorozat – ebben a sorrendben, esetleg
  #   közbeszúrt 0-kal – n-szer ismétlődik,
  # * len-n*m számú helyen 0-k vannak,
  # * a constraints korlát-listában felsorolt indexű cellákban a megadott
  #   értékű elemek vannak.
  def cyclists({n, m, len}, constraints) do    
    backtrack(m, m, len, len-m*n, [], Map.new(constraints))
  end

  @spec putZeros(non::integer(), data::%{}, m::integer(), curr::integer()) :: results::[[value()]]
  # visszaadja a nullások számát
  defp putZeros(non, [], _, _) do
    non
  end
  # ezeket kihagyja, mert a nullás korlátokkal nincs dolgunk itt
  defp putZeros(non, [{_, 0} | rest], m, curr) do
    putZeros(non, rest, m, curr)
  end
  # visszadja, hogy curr-ig mennyi darab nullást kell lerakni minimum
  defp putZeros(non, [{cidx, val} | rest], m, curr) do
    zz = non + Integer.mod((cidx - val - non), m)
    if cidx <= curr do
      putZeros(zz, rest, m, curr)
    else
      non
    end
  end
  
  @spec appender(val::integer(), m::integer(), len::integer(), non::integer(), data::%{}, cts::%{}, element::integer()) :: results::[[value()]]
  # a lista elejére beteszi az element értékét, ami vagy egy 1..m szám, vagy egy nullás
  defp appender(val, m, len, non, data, cts, element) do
    if (cts[len] == nil or cts[len] == element) 
      and (putZeros(0, cts |> Enum.to_list |> Enum.sort, m, len) < non+ 1) 
    do
      backtrack(val, m, len - 1, non, [element | data], cts)
    else
      []
    end
  end

  @spec backtrack(val::integer(), m::integer(), len::integer(), non::integer(), data::%{}, cts::%{}) :: results::[[value()]]
  # ha felépült a lista, és elfogytak a lerakandó nullák, akkor vissztér a listával
  defp backtrack(_, _, 0, _, data, _) do
    [data]
  end
  # megpróbálja felépíteni a listát hátúlról, és ha egy korlát nem teljesül, 
  # akkor eldobja a próbálkozát
  defp backtrack(val, m, len, non, data, cts) do
    nextval = if val == 1 do 
      m
    else 
      val - 1
    end

    cond do
      non == len -> appender(val, m, len, non - 1, data, cts, 0)
      non == 0 -> appender(nextval, m, len, non, data, cts, val)
      true -> appender(val, m, len, non - 1, data, cts, 0) ++ appender(nextval, m, len, non, data, cts, val)
    end
  end
end
# Khf3.cyclists({3, 2, 7}, [{4, 0}])  |> IO.inspect

defmodule Nhf1 do
  @moduledoc """
  Számtekercs
  @author "Koczó Attila koczoa@edu.bme.hu"
  @date   "2024-10-10"
  ...
  """
      @type size()  :: integer() # tábla mérete (0 < n)
      @type cycle() :: integer() # ciklus hossza (0 < m <= n)
      @type value() :: integer() # mező értéke (0 < v <= m)

      @type row()   :: integer()       # sor száma (1-től n-ig)
      @type col()   :: integer()       # oszlop száma (1-től n-ig)
      @type field() :: {row(), col()}  # mező koordinátái

      @type field_value() :: {field(), value()}                 # mező és értéke
      @type puzzle_desc() :: {size(), cycle(), [field_value()]} # feladvány

      @type retval()    :: integer()    # eredménymező értéke (0 <= rv <= m)
      @type solution()  :: [[retval()]] # egy megoldás
      @type solutions() :: [solution()] # összes megoldás

      @spec helix(sd::puzzle_desc()) :: ss::solutions()
      # ss az sd feladványleíróval megadott feladvány összes megoldásának listája
      def helix({n, m, costs}) do 
        solve(Khf2.balra(1, 1, n, 1, Enum.into(costs, %{})),  to2d(n, costs), 1, m)
      end

      defp to2d(n, consts) do
        cs = Map.new(consts)
        for y <- 1..n do
          for x <- 1..n do
            cs[{y, x}]
          end
        end          
      end

      defp solve([], table, _, _) do
        [table]
      end

      defp solve([{{y, x}, val} | rest], table, curr, m) do
        cond do
          val == curr ->  solve(rest, table, currSetter(curr, m), m)
          val == nil ->
            neuTable = replaceIdx(table, {y, x}, curr)
            a = if checkConst(neuTable, {y, x}, m) do
              solve(rest, neuTable, currSetter(curr, m), m)
            else
              []
            end
            neuTable = replaceIdx(table, {y, x}, 0)
            b = if checkConst(neuTable, {y, x}, m) do
              solve(rest, neuTable, curr, m)
            else
              []
            end
            a++b
          true -> []
        end
      end


      defp checkConst(table, {y, x}, m) do
        row = Enum.at(table, y-1)
        col = Enum.map(table, fn row -> Enum.at(row, x-1) end)
        val = idx(table, {y, x})
        if val == 0 do
          xdrow = Enum.count(row, fn a -> a != 0 end) >= m
          xdcol = Enum.count(col, fn a -> a != 0 end) >= m
          xdrow and xdcol
        else
          rc = Enum.count(row, fn a -> a == val end) == 1
          cc = Enum.count(col, fn a -> a == val end) == 1
          rc and cc
        end
        # rc and cc and xdrow and xdcol
      end
      
      defp currSetter(curr, m) do
        if curr == m do
          1
        else
          curr + 1
        end
      end

      defp replaceIdx(table, {y, x}, val) do
        List.update_at(table, y-1, fn oldRow -> List.replace_at(oldRow, x-1, val) end)
      end

      defp idx(table, {y, x}) do
       Enum.at(Enum.at(table, y-1), x-1)
      end
end
# Nhf1.helix({4, 2, [{{1, 1}, 1}, {{1, 4}, 2}]}) |> IO.inspect()
# Nhf1.helix({6, 3, [{{1,5},2},{{2,2},1},{{4,6},1}]}) |> IO.inspect
# Khf2.helix(["6", "3", "1 5 2", "2 2 1", "4 6 1"]) |> IO.inspect()

defmodule Nhf1Testcases do

  testcases = # %{key => {size, cycle, constraints, solutions}}
    %{
      0 => {3, 2, [], [[[0, 1, 2], [1, 2, 0], [2, 0, 1]], [[0, 1, 2], [2, 0, 1], [1, 2, 0]], [[1, 2, 0], [2, 0, 1], [0, 1, 2]]]},
      1 => {4, 2, [{{1, 1}, 1}, {{1, 4}, 2}], [[[1, 0, 0, 2], [0, 1, 2, 0], [0, 2, 1, 0], [2, 0, 0, 1]], [[1, 0, 0, 2], [2, 0, 0, 1], [0, 2, 1, 0], [0, 1, 2, 0]], [[1, 0, 0, 2], [2, 0, 1, 0], [0, 2, 0, 1], [0, 1, 2, 0]]]},
      2 => {4, 1, [{{1, 1}, 1}], [[[1, 0, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0], [0, 1, 0, 0]], [[1, 0, 0, 0], [0, 0, 0, 1], [0, 1, 0, 0], [0, 0, 1, 0]], [[1, 0, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1], [0, 1, 0, 0]], [[1, 0, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 0, 1]], [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]], [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]]},
      3 => {4, 3, [], []},
      4 => {5, 3, [{{1, 3}, 1}, {{2, 2}, 2}], [[[0, 0, 1, 2, 3], [0, 2, 0, 3, 1], [1, 3, 0, 0, 2], [3, 0, 2, 1, 0], [2, 1, 3, 0, 0]], [[0, 0, 1, 2, 3], [0, 2, 3, 0, 1], [1, 3, 0, 0, 2], [3, 0, 2, 1, 0], [2, 1, 0, 3, 0]]]},
      5 => {6, 3, [{{1, 5}, 2}, {{2, 2}, 1}, {{4, 6}, 1}], [[[1, 0, 0, 0, 2, 3], [0, 1, 2, 3, 0, 0], [0, 3, 1, 2, 0, 0], [0, 2, 3, 0, 0, 1], [3, 0, 0, 0, 1, 2], [2, 0, 0, 1, 3, 0]]]},
      6 => {6, 3, [{{1, 5}, 2}, {{2, 2}, 1}, {{4, 6}, 1}], [[[1, 0, 0, 0, 2, 3], [0, 1, 2, 3, 0, 0], [0, 3, 1, 2, 0, 0], [0, 2, 3, 0, 0, 1], [3, 0, 0, 0, 1, 2], [2, 0, 0, 1, 3, 0]]]},
      7 => {6, 3, [{{2, 4}, 3}, {{3, 3}, 1}, {{3, 6}, 2}, {{6, 1}, 3}], [[[0, 1, 2, 0, 3, 0], [2, 0, 0, 3, 0, 1], [0, 3, 1, 0, 0, 2], [0, 0, 3, 2, 1, 0], [1, 0, 0, 0, 2, 3], [3, 2, 0, 1, 0, 0]]]},
      8 => {7, 3, [{{1, 1}, 1}, {{2, 4}, 3}, {{3, 4}, 1}, {{4, 3}, 3}, {{6, 6}, 2}, {{7, 7}, 3}], [[[1, 0, 0, 2, 0, 3, 0], [0, 1, 2, 3, 0, 0, 0], [0, 3, 0, 1, 2, 0, 0], [0, 2, 3, 0, 0, 0, 1], [3, 0, 0, 0, 0, 1, 2], [0, 0, 1, 0, 3, 2, 0], [2, 0, 0, 0, 1, 0, 3]]]},
      9 => {8, 3, [{{1, 4}, 1}, {{1, 7}, 3}, {{2, 3}, 2}, {{2, 4}, 3}, {{3, 2}, 1}, {{4, 7}, 1}, {{7, 7}, 2}], [[[0, 0, 0, 1, 0, 2, 3, 0], [0, 0, 2, 3, 0, 0, 0, 1], [0, 1, 0, 0, 2, 3, 0, 0], [0, 3, 0, 0, 0, 0, 1, 2], [1, 2, 3, 0, 0, 0, 0, 0], [3, 0, 0, 2, 0, 1, 0, 0], [0, 0, 1, 0, 3, 0, 2, 0], [2, 0, 0, 0, 1, 0, 0, 3]], [[0, 0, 0, 1, 0, 2, 3, 0], [0, 0, 2, 3, 0, 0, 0, 1], [0, 1, 0, 0, 2, 3, 0, 0], [0, 3, 0, 0, 0, 0, 1, 2], [1, 2, 3, 0, 0, 0, 0, 0], [3, 0, 0, 2, 1, 0, 0, 0], [0, 0, 1, 0, 3, 0, 2, 0], [2, 0, 0, 0, 0, 1, 0, 3]], [[0, 0, 0, 1, 0, 2, 3, 0], [0, 0, 2, 3, 0, 0, 0, 1], [0, 1, 0, 2, 0, 3, 0, 0], [0, 3, 0, 0, 0, 0, 1, 2], [1, 2, 3, 0, 0, 0, 0, 0], [3, 0, 0, 0, 2, 1, 0, 0], [0, 0, 1, 0, 3, 0, 2, 0], [2, 0, 0, 0, 1, 0, 0, 3]], [[0, 0, 0, 1, 0, 2, 3, 0], [0, 0, 2, 3, 0, 0, 0, 1], [0, 1, 0, 2, 3, 0, 0, 0], [0, 3, 0, 0, 0, 0, 1, 2], [1, 2, 3, 0, 0, 0, 0, 0], [3, 0, 0, 0, 2, 1, 0, 0], [0, 0, 0, 0, 1, 3, 2, 0], [2, 0, 1, 0, 0, 0, 0, 3]], [[0, 0, 0, 1, 0, 2, 3, 0], [0, 0, 2, 3, 0, 0, 0, 1], [0, 1, 0, 2, 3, 0, 0, 0], [0, 3, 0, 0, 0, 0, 1, 2], [1, 2, 3, 0, 0, 0, 0, 0], [3, 0, 0, 0, 2, 1, 0, 0], [0, 0, 1, 0, 0, 3, 2, 0], [2, 0, 0, 0, 1, 0, 0, 3]]]},
      10 => {8, 4, [{{2, 3}, 4}, {{3, 3}, 2}, {{6, 1}, 1}, {{7, 6}, 3}], [[[0, 0, 1, 2, 3, 4, 0, 0], [0, 0, 4, 0, 1, 2, 3, 0], [0, 0, 2, 3, 0, 0, 4, 1], [3, 1, 0, 4, 0, 0, 0, 2], [2, 4, 0, 0, 0, 0, 1, 3], [1, 3, 0, 0, 0, 0, 2, 4], [0, 2, 0, 1, 4, 3, 0, 0], [4, 0, 3, 0, 2, 1, 0, 0]], [[0, 0, 1, 2, 3, 4, 0, 0], [0, 0, 4, 0, 1, 2, 3, 0], [3, 0, 2, 0, 0, 0, 4, 1], [0, 1, 3, 4, 0, 0, 0, 2], [2, 4, 0, 0, 0, 0, 1, 3], [1, 3, 0, 0, 0, 0, 2, 4], [0, 2, 0, 1, 4, 3, 0, 0], [4, 0, 0, 3, 2, 1, 0, 0]]]},
      11 => {9, 3, [{{1, 7}, 3}, {{3, 1}, 1}, {{6, 1}, 3}, {{6, 2}, 2}, {{6, 6}, 1}, {{8, 4}, 3}, {{9, 2}, 1}], [[[0, 0, 0, 0, 1, 2, 3, 0, 0], [0, 0, 2, 0, 0, 0, 0, 3, 1], [1, 3, 0, 0, 0, 0, 0, 0, 2], [0, 0, 0, 1, 2, 3, 0, 0, 0], [0, 0, 0, 2, 3, 0, 1, 0, 0], [3, 2, 0, 0, 0, 1, 0, 0, 0], [0, 0, 3, 0, 0, 0, 2, 1, 0], [0, 0, 1, 3, 0, 0, 0, 2, 0], [2, 1, 0, 0, 0, 0, 0, 0, 3]]]}
    }
  for i <- 0..map_size(testcases)-1
    do
    {size, cycle, constrains, solutions} = testcases[i]
    {"Test case #{i}",
     Nhf1.helix({size, cycle, constrains}) |> Enum.sort() === solutions
    }
    |> IO.inspect
  end

end
