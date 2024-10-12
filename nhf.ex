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

  

  # @spec correct?(pd::puzzle_desc()) :: b::boolean()
  # # b igaz, ha a pd feladványleíró helyes, egyébként hamis
  
  # def correct?({n, m, fields}) do 
  #   0 < n 
  #   and 0 < m 
  #   and m <= n
  #   and correct_fields(n, m, fields)
  #   and Enum.frequencies_by(fields,
  #     fn {{row, col}, _} -> {row, col} end)
  #     |> Map.values()
  #     |> Enum.all?(fn x -> x == 1 end)
  # end


  # @spec correct_fields(n::integer(), m::integer(), pd::puzzle_desc()) :: b::boolean()
  # # b igaz, ha az alábbi feltételek teljesülnek:
  # # 0 < n, 0 < m ≤ n
  # # 0 < row ≤ n, 0 < col ≤ n
  # # 0 < val ≤ m
  
  # defp correct_fields(_, _, []) do
  #   true
  # end

  # defp correct_fields(n, m, [{{row, col}, val} | rest]) do
  #   0 < row
  #   and row <= n
  #   and 0 < col 
  #   and col <= n
  #   and 0 < val
  #   and val <= m
  #   and correct_fields(n, m, rest)
  # end
    
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
      
      # @spec to2d(n::integer(), consts::[index_value()]) :: %{}
      # előállít egy n*n-es "mátrixot", és a consts értékeit beleteszi
      defp to2d(n, consts) do
        cs = Map.new(consts)
        for y <- 1..n do
          for x <- 1..n do
            cs[{y, x}]
          end
        end          
      end

      # @spec solve(csiga::[field_opt_value()] , table::%{}, curr::integer(), m::integer()) :: solutions()
      # kitölti a table-t a megkapott csiga értékekkel
      defp solve([], table, _, _) do
        [table]
      end


      defp solve([{{y, x}, val} | rest], table, curr, m) do
        case val do
          ^curr -> solve(rest, table, currSetter(curr, m), m)
          nil ->
            a = if checkConst(table, {y, x}, m, curr) do
              neuTable = replaceIdx(table, {y, x}, curr)
              solve(rest, neuTable, currSetter(curr, m), m)
            else
              []
            end
            b = if checkConst(table, {y, x}, m, 0) do
              neuTable = replaceIdx(table, {y, x}, 0)
              solve(rest, neuTable, curr, m)
            else
              []
            end
            a++b
          _ -> []
        end
      end


      # @spec checkConst(table::%{}, coords::field(), m::integer(), val::integer()) ::boolean()
      # ellenőrzi, hogy a val értéke lerakható-e a coords-ban kapott helyre a table-ben
      defp checkConst(table, {y, x}, m, val) do
        row = Enum.at(table, y-1)
        if val == 0 do
          xdrow = Enum.count(row, fn a -> a != 0 end) >= m + 1
          xdcol = Enum.count(table, fn row -> Enum.at(row, x-1) !== 0 end) >= m + 1 
          xdrow and xdcol
        else
          rc = Enum.all?(row, fn a -> a !== val end)
          cc = Enum.all?(table, fn row -> Enum.at(row, x-1) !== val end)
          rc and cc
        end
      end
      
      # @spec(curr::integer(), m::interger())
      # növeli egyel a curr értékét, amíg az el nem éri m-et, különben egyre állítja 
      defp currSetter(curr, m) do
        if curr == m do
          1
        else
          curr + 1
        end
      end
      # @spec replaceIdx(table::%{}, coords::field(), val::integer()) ::%{}
      # A táblában az x, y koordinátán álló számot kicseréli a val értékére
      defp replaceIdx(table, {y, x}, val) do
        List.update_at(table, y-1, fn oldRow -> List.replace_at(oldRow, x-1, val) end)
      end
end
