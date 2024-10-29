------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                          PORT VISIBILITY                                 --
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

package et_port_visibility is


	keyword_terminal_name_visible	: constant string := "terminal_name_visible";
	keyword_port_name_visible		: constant string := "port_name_visible";
	
	
	type type_port_name_visible is (YES, NO);
	port_name_visible_default : constant type_port_name_visible := YES;
	function to_string (visible : in type_port_name_visible) return string;
	function to_port_name_visible (visible : in string) return type_port_name_visible;	

	
	type type_terminal_name_visible is (YES, NO);
	terminal_name_visible_default : constant type_terminal_name_visible := YES;
	function to_string (visible : in type_terminal_name_visible) return string;	
	function to_terminal_name_visible (visible : in string) return type_terminal_name_visible;

		
	
end et_port_visibility;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
