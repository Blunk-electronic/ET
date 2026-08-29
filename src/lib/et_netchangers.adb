------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        NETCHANGERS GENERAL                               --
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

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo:



-- with ada.text_io;			use ada.text_io;
with ada.strings;					use ada.strings;
with ada.strings.fixed;				use ada.strings.fixed;



package body et_netchangers is


	function to_netchanger_id (
		id : in string)
		return type_netchanger_id
	is (type_netchanger_id'value (id));



	function to_string (
		id : in type_netchanger_id)
		return string
	is (trim (type_netchanger_id'image (id), left));




	function get_netchanger_name (
		id : in type_netchanger_id)
		return string
	is (netchanger_prefix & to_string (id));




	procedure toggle_direction (
		direction : in out type_netchanger_direction)
	is begin
		case direction is
			when FORWARD =>
				direction := BACKWARD;

			when BACKWARD =>
				direction := FORWARD;
		end case;
	end toggle_direction;




	function to_netchanger_direction (
		direction : in string)
		return type_netchanger_direction
	is (type_netchanger_direction'value (direction));


	function to_string (
		direction : in type_netchanger_direction)
		return string
	is (type_netchanger_direction'image (direction));













	procedure reset_netchanger (
		netchanger : in out type_netchanger)
	is begin
		netchanger.position_sch := (others => <>);
		netchanger.position_brd := (others => <>);
		netchanger.direction := type_netchanger_direction'first;
	end reset_netchanger;





	procedure copy_netchanger_with_offset (
		netchanger_in	: in type_netchanger;
		offset			: in type_object_position_relative;
		netchanger_out	: out type_netchanger)
	is
		procedure reset_board_position is
			use et_board_geometry;
			use pac_geometry_2;
		begin
			netchanger_out.position_brd.place := origin;
		end reset_board_position;


		-- Moves netchanger_out by the given
		-- offset:
		-- - x/y position
		-- - sheet
		procedure set_schematic_position is
			use et_sheets;
		begin
			-- sheet:
			add (netchanger_out.position_sch.sheet, get_sheet (offset));

			-- place (x/y):
			-- CS
		end set_schematic_position;


	begin
		netchanger_out := netchanger_in;

		-- Reset board position to default:
		reset_board_position;

		-- Reset status flags:
		reset_status (netchanger_out.status_sch);
		reset_status (netchanger_out.status_brd);

		-- Set the schematic position:
		set_schematic_position;
	end copy_netchanger_with_offset;





	function get_netchanger_name (
		netchanger_cursor : in pac_netchangers.cursor)
		return string
	is (get_netchanger_name (key (netchanger_cursor)));



	function get_netchanger_id (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_netchanger_id
	is (key (netchanger_cursor));



	function get_netchanger (
		netchangers : in pac_netchangers.map;
		index		: in type_netchanger_id)
		return pac_netchangers.cursor
	is (netchangers.find (index));




end et_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
