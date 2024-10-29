------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           PORT STRENGTH                                  --
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

package et_port_strength is


	type type_output_weakness is (
		NONE, -- push-pull
		WEAK0, WEAK1, -- requires external pull-down/up resistor
		PULL0, PULL1  -- internal pull-down/up resistor
		);

	output_weakness_default : constant type_output_weakness := NONE;
	function to_string (weakness : in type_output_weakness) return string;
	function to_output_weakness (weakness : in string) return type_output_weakness;

	type type_output_tristate is (NO, YES);
	output_tristate_default : constant type_output_tristate := NO;
	function to_string (tristate : in type_output_tristate) return string;
	function to_output_tristate (tristate : in string) return type_output_tristate;

	

	keyword_weakness				: constant string := "weakness";
	keyword_tristate				: constant string := "tristate";	

	keyword_output_inverted			: constant string := "output_inverted";
	keyword_output_weakness			: constant string := "output_weakness";
	keyword_output_tristate			: constant string := "output_tristate";

	
	
end et_port_strength;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
