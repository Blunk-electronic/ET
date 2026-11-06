------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            LISTS OF VIAS                                 --
--                                                                          --
--                              B o d y                                     --
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

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

-- with ada.text_io;			use ada.text_io;

package body et_vias is



	function get_position (
		via : in pac_vias.cursor)
		return string
	is (get_position (element (via)));



	function get_place (
		via : in pac_vias.cursor)
		return type_vector_model
	is (get_place (element (via)));





	function is_selected (
		via : in pac_vias.cursor)
		return boolean
	is (is_selected (element (via)));


	function is_proposed (
		via : in pac_vias.cursor)
		return boolean
	is (is_proposed (element (via)));


	function is_moving (
		via : in pac_vias.cursor)
		return boolean
	is (is_moving (element (via)));




	procedure iterate (
		vias	: in pac_vias.list;
		process	: not null access procedure (position : in pac_vias.cursor);
		proceed	: not null access boolean)
	is
		use pac_vias;
		c : pac_vias.cursor := vias.first;
	begin
		while c /= pac_vias.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;



end et_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
