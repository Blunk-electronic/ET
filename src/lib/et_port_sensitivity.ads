------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          PORT SENSITIVITY                                --
--                                                                          --
--                              S p e c                                     --
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

package et_port_sensitivity is


	-- Sensitity of inputs:
	type type_sensitivity_edge is (
		NONE, 		-- passive and analog
		RISING,		-- digital
		FALLING,	-- digital
		ANY			-- digtial
		);
	
	sensitivity_edge_default : constant type_sensitivity_edge := NONE;
	function to_string (sensitivity : in type_sensitivity_edge) return string;
	function to_sensitivity_edge (sensitivity : in string) return type_sensitivity_edge;

	type type_sensitivity_level is (NONE, LOW, HIGH); -- CS NONE required ?
	sensitivity_level_default : constant type_sensitivity_level := HIGH; -- CS good idea ?
	function to_string (sensitivity : in type_sensitivity_level) return string;
	function to_sensitivity_level (sensitivity : in string) return type_sensitivity_level;


	keyword_sensitivity_edge		: constant string := "sensitivity_edge";
	keyword_sensitivity_level		: constant string := "sensitivity_level";

	keyword_input_sensitivity_edge	: constant string := "input_sensitivity_edge";
	keyword_input_sensitivity_level	: constant string := "input_sensitivity_level";


	
	
end et_port_sensitivity;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
