------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          DEVICE RENUMBERING                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
-- To Do: 
--
--


package body et_device_renumbering is

	

	function "<" (left, right : in type_renumber_module) return boolean is
		use pac_module_name;
		use pac_module_instance_name;
		result : boolean;
	begin
		if left.name < right.name then
			result := true;
		elsif left.name > right.name then
			result := false;
		else -- names equal
			if left.instance < right.instance then
				result := true;
			elsif left.instance > right.instance then
				result := false;
			else -- instances equal
				result := false;
			end if;
		end if;

		return result;
	end "<";



	
	function to_index_range (
		module_name	: in pac_module_name.bounded_string;
		index_range	: in type_index_range) return string is
	begin
		return ("module " & enclose_in_quotes (to_string (module_name)) &
			" range" & to_string (index_range.lowest) &
			" .." & to_string (index_range.highest));
	end to_index_range;



	
	function below (left, right : in type_index_range) return boolean is begin
		if left.highest < right.lowest then
			return true;
		else
			return false;
		end if;
	end;



	
	function above (left, right : in type_index_range) return boolean is begin
		if left.lowest > right.highest then
			return true;
		else
			return false;
		end if;
	end;



	function get_first_child_submodule (
		submodules	: in pac_renumber_modules.tree)
		return pac_renumber_modules.cursor
	is
		result : pac_renumber_modules.cursor;
	begin
		-- Get to the root of the submodules tree:
		result := submodules.root;

		-- Get the first child submodule:
		return first_child (result);
	end;


	

	function get_module_count (
		modules	: in pac_renumber_modules.tree)
		return count_type
	is begin
		return modules.node_count;
	end;


	function get_module_count (
		modules	: in pac_renumber_modules.tree)
		return string
	is begin
		return count_type'image (modules.node_count);
	end;


	
end et_device_renumbering;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
