------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON SHEETS                        --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--


with ada.text_io;					use ada.text_io;


package body et_schematic_ops.sheets is


	function get_sheet_description (
		module	: in pac_generic_modules.cursor;
		sheet	: in type_sheet)
		return type_schematic_description 
	is

		use pac_schematic_descriptions;
		cursor : pac_schematic_descriptions.cursor;

		use et_schematic;

		
		procedure query_descriptions (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			cursor := find (module.frames.descriptions, sheet);
		end query_descriptions;

		
	begin
		query_element (
			position	=> module,
			process		=> query_descriptions'access);
		
		if cursor /= pac_schematic_descriptions.no_element then
			return element (cursor);
		else
		-- If the sheet has no description, then return the defaults.
			return (others => <>);
		end if;
	end get_sheet_description;

	
end et_schematic_ops.sheets;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
