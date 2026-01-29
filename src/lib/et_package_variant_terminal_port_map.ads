------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    PACKAGE VARIANT / TERMINAL-PORT-MAP                   --
--                                                                          --
--                              S p e c                                     --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--


with ada.containers; 			use ada.containers;
with ada.containers.ordered_maps;

with et_terminal_name;			use et_terminal_name;
with et_port_names;				use et_port_names;
with et_unit_name;				use et_unit_name;


package et_package_variant_terminal_port_map is


	type type_port_in_terminal_port_map is record
		name	: pac_port_name.bounded_string; -- CLK, CE, VSS -- CS rename to port
		unit	: pac_unit_name.bounded_string; -- GPIO_BANK_3
	end record;



	
	package pac_terminal_port_map is new ordered_maps (
		key_type 		=> pac_terminal_name.bounded_string, -- H7, 14
		"<"				=> pac_terminal_name."<",
		element_type 	=> type_port_in_terminal_port_map); -- unit A, OE1



	procedure dummy;
	
end et_package_variant_terminal_port_map;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
