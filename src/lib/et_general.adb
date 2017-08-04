------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET GENERAL DECLARATIONS                        --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
with ada.strings.unbounded; 	use ada.strings.unbounded;

package body et_general is

	function to_component_reference (text_in : in string) return type_component_reference is
	-- Converts a string like "IC303" to a type_component_reference.
	-- CS: check if allowed prefix
		r : type_component_reference;
	begin
		r.prefix := et_general.type_component_prefix.to_bounded_string("N");
		r.id := 1;
		
		return r;
	end to_component_reference;
	
	function component_reference_compare ( left, right : in type_component_reference) return boolean is
		-- CS
	begin
		return true;
	end component_reference_compare;
	
	function text_meaning_to_string ( meaning : in type_text_meaning) return string is
	-- Converts meaning to string.
		function strip_prefix ( s : in string) return string is
		begin
			return s (s'first + text_meaning_prefix'last .. s'last);
		end strip_prefix;
	begin
		-- Since the text meaning may assume p_function 
		-- (which is a workaround in order not to use an ada keyword. see spec of this package),
		-- we must get gid of the prefix:
		case meaning is
			when p_function => -- we must remove the prefix
				return strip_prefix(to_lower(type_text_meaning'image(meaning)));
			when others => -- we can do a direct conversion
				return to_lower(type_text_meaning'image(meaning));
		end case;
	end text_meaning_to_string;
	
	-- GENERICS
	
	package body stack_lifo is
		s : array (1..max) of item;
		top : natural range 0..max;

		procedure push(x : item) is
		begin
			top := top + 1;
			s(top) := x;
		end push;

		function pop return item is
		begin
			top := top - 1;
			return s(top + 1);
		end pop;

		function depth return natural is
		begin
			return top;
		end depth;

		procedure init is
		begin
			top := 0;
		end init;
	
	end stack_lifo;

end et_general;

