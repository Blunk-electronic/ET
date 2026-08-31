------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         NETCHANGERS / SCHEMATIC                          --
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
with ada.characters;				use ada.characters;
with ada.characters.handling;		use ada.characters.handling;
with ada.strings;					use ada.strings;
with ada.strings.fixed;				use ada.strings.fixed;




package body et_netchangers.schematic is






-- PORT NAMES:

	function to_port_name (
		name : in string)
		return type_netchanger_port_name
	is (type_netchanger_port_name'value (name));



	function to_string (
		name : in type_netchanger_port_name)
		return string
	is (trim (to_lower (type_netchanger_port_name'image (name)), left));




	function to_short_name (
		direction : in type_netchanger_port_name)
		return string
	is begin
		case direction is
			when MASTER => return port_short_master;
			when SLAVE => return port_short_slave;
		end case;
	end to_short_name;





	function get_opposide_port (
		port : in type_netchanger_port_name)
		return type_netchanger_port_name
	is begin
		case port is
			when MASTER => return SLAVE;
			when SLAVE  => return MASTER;
		end case;
	end get_opposide_port;




	function get_direction (
		netchanger : in type_netchanger)
		return type_netchanger_direction
	is (netchanger.direction);



	procedure set_direction (
		netchanger	: in out type_netchanger;
		direction	: in type_netchanger_direction)
	is begin
		netchanger.direction := direction;
	end set_direction;



	procedure toggle_direction (
		netchanger	: in out type_netchanger)
	is begin
		case netchanger.direction is
			when FORWARD =>
				netchanger.direction := BACKWARD;

			when BACKWARD =>
				netchanger.direction := FORWARD;
		end case;
	end toggle_direction;




	function to_position (
		position : in type_netchanger_position_schematic)
		return type_position
	is
		result : type_position;
	begin
		result.place := position.place;
		result.rotation := position.rotation;

		return result;
	end to_position;




	procedure set_place (
		position	: in out type_netchanger_position_schematic;
		place		: in type_vector_model)
	is begin
		position.place := place;
	end set_place;



	procedure move_by (
		position	: in out type_netchanger_position_schematic;
		offset		: in type_vector_model)
	is begin
		add (position.place, offset);
	end move_by;




	function get_place (
		position	: in type_netchanger_position_schematic)
		return type_vector_model
	is (position.place);




	procedure set_sheet (
		position	: in out type_netchanger_position_schematic;
		sheet		: in type_sheet)
	is begin
		position.sheet := sheet;
	end set_sheet;



	procedure add_sheet (
		position	: in out type_netchanger_position_schematic;
		sheet		: in type_sheet_relative)
	is begin
		add (position.sheet, sheet);
	end add_sheet;



	function get_sheet (
		position : in type_netchanger_position_schematic)
		return type_sheet
	is (position.sheet);






	procedure move_netchanger (
		netchanger	: in out type_netchanger;
		offset		: in type_vector_model)
	is begin
		move_by (netchanger.position_sch, offset);
	end move_netchanger;


	procedure move_netchanger (
		netchanger	: in out type_netchanger;
		offset		: in type_sheet_relative)
	is begin
		add (netchanger.position_sch.sheet, offset);
	end move_netchanger;






	function get_rotation (
		position : in type_netchanger_position_schematic)
		return type_rotation_0_90
	is (position.rotation);



	function to_netchanger_position (
		position : in type_object_position)
		return type_netchanger_position_schematic
	is
		result : type_netchanger_position_schematic;
	begin
		result.place := get_place (position);
		result.rotation := get_rotation (position);
		result.sheet := get_sheet (position);
		return result;
	end to_netchanger_position;



	function to_netchanger_position (
		sheet		: in et_sheets.type_sheet;
		place		: in et_schematic_geometry.pac_geometry_2.type_vector_model;
		rotation	: in et_schematic_geometry.pac_geometry_2.type_rotation_0_90)
		return type_netchanger_position_schematic
	is ((place, rotation, sheet));



	function to_object_position (
		position : in type_netchanger_position_schematic)
		return type_object_position
	is
		result : type_object_position;
	begin
		set_place (result, position.place);
		set_rotation (result, position.rotation);
		set_sheet (result, position.sheet);

		return result;
	end to_object_position;




	function to_string (
		position	: in type_netchanger_position_schematic)
		return string
	is
		use et_schematic_coordinates;
		p : type_object_position;
	begin
		p := to_object_position (position);
		return to_string (p);
	end to_string;




	function get_object_position (
		netchanger : in type_netchanger)
		return type_object_position
	is (to_object_position (netchanger.position_sch));




	function get_position (
		netchanger : in type_netchanger)
		return type_netchanger_position_schematic
	is (netchanger.position_sch);





	procedure set_position (
		netchanger	: in out type_netchanger;
		position	: in type_netchanger_position_schematic)
	is begin
		netchanger.position_sch := position;
	end set_position;



	procedure set_place (
		netchanger	: in out type_netchanger;
		place		: in type_vector_model)
	is begin
		netchanger.position_sch.place := place;
	end set_place;



	function get_place (
		netchanger	: in type_netchanger)
		return type_vector_model
	is begin
		return netchanger.position_sch.place;
	end get_place;




	function get_position_schematic (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_netchanger_position_schematic
	is
		n : type_netchanger renames element (netchanger_cursor);
	begin
		return get_position (n);
	end get_position_schematic;





	function get_rotation (
		netchanger	: in type_netchanger)
		return type_rotation_0_90
	is (netchanger.position_sch.rotation);


	procedure set_rotation (
		netchanger	: in out type_netchanger;
		rotation	: in type_rotation_0_90)
	is begin
		netchanger.position_sch.rotation := rotation;
	end set_rotation;


	procedure toggle_rotation (
		netchanger	: in out type_netchanger)
	is begin
		toggle_rotation (netchanger.position_sch.rotation);
	end toggle_rotation;




	function get_sheet (
		netchanger	: in type_netchanger)
		return type_sheet
	is (netchanger.position_sch.sheet);



	procedure set_sheet (
		netchanger	: in out type_netchanger;
		sheet		: in type_sheet)
	is begin
		netchanger.position_sch.sheet := sheet;
	end set_sheet;





	function get_rotation (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_rotation_0_90
	is
		netchanger : type_netchanger renames element (netchanger_cursor);
	begin
		return get_rotation (netchanger);
	end get_rotation;




	function get_direction (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_netchanger_direction
	is
		netchanger : type_netchanger renames element (netchanger_cursor);
	begin
		return get_direction (netchanger);
	end get_direction;




	function get_sheet (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_sheet
	is
		n : type_netchanger renames element (netchanger_cursor);
	begin
		return n.position_sch.sheet;
	end get_sheet;







-- PORTS:


	procedure swap_ports (
		ports : in out type_netchanger_ports)
	is
		master_bak : constant type_vector_model := ports.master;
	begin
		ports.master := ports.slave;
		ports.slave := master_bak;
	end swap_ports;





	function get_netchanger_ports (
		netchanger : in type_netchanger)
		return type_netchanger_ports
	is
		-- CS: double code. see function get_netchanger_ports
		-- below. Use a separate subprogram for both
		-- functions.

		n : type_netchanger renames netchanger;

		ports : type_netchanger_ports;
	begin
		if n.direction = BACKWARD then
			swap_ports (ports);
		end if;

		-- rotate the ports according to rotation in schematic
		rotate_by (ports.master, n.position_sch.rotation);
		rotate_by (ports.slave,  n.position_sch.rotation);

		-- move the ports according to position in schematic
		move_by (ports.master, n.position_sch.place);
		move_by (ports.slave,  n.position_sch.place);

		return ports;
	end get_netchanger_ports;






	function get_netchanger_ports (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_netchanger_ports
	is
		use pac_netchangers;
		n : type_netchanger renames element (netchanger_cursor);

		ports : type_netchanger_ports;
	begin
		if n.direction = BACKWARD then
			swap_ports (ports);
		end if;

		-- rotate the ports according to rotation in schematic
		rotate_by (ports.master, n.position_sch.rotation);
		rotate_by (ports.slave,  n.position_sch.rotation);

		-- move the ports according to position in schematic
		move_by (ports.master, n.position_sch.place);
		move_by (ports.slave,  n.position_sch.place);

		return ports;
	end get_netchanger_ports;







-- CATCH ZONE:

	function in_catch_zone (
		netchanger	: in type_netchanger;
		zone		: in type_catch_zone;
		sheet		: in type_sheet)
		return boolean
	is
		result : boolean := false;

		netchanger_position : constant type_object_position :=
			to_object_position (netchanger.position_sch);
	begin
		-- The netchanger must be on the given sheet and
		-- in the given catch zone:
		if	get_sheet (netchanger_position) = sheet
		and	in_catch_zone (zone, get_place (netchanger_position)) then
			result := true;
		else
			result := false;
		end if;

		return result;
	end in_catch_zone;






-- AREA:

	function on_sheet_and_in_area (
		netchanger	: in type_netchanger;
		sheet		: in type_sheet;
		area		: in type_area)
		return boolean
	is
		result : boolean := false;
		place : type_vector_model;
	begin
		-- The given netchanger must be on the given sheet:
		if get_sheet (netchanger) = sheet then

			-- Get the x/y position of the netchanger:
			place := get_place (get_position (netchanger));

			-- The netchanger must be in the given area:
			if in_area (place, area) then
				result := true;
			else
				result := false;
			end if;

		else
			result := false;
		end if;

		return result;
	end on_sheet_and_in_area;










-- STATUS:


	function is_selected (
		netchanger : in type_netchanger)
		return boolean
	is (is_selected (netchanger.status_sch));



	function is_proposed (
		netchanger : in type_netchanger)
		return boolean
	is (is_proposed (netchanger.status_sch));



	function is_moving (
		netchanger : in type_netchanger)
		return boolean
	is (is_moving (netchanger.status_sch));



	procedure set_moving (
		netchanger : in out type_netchanger)
	is begin
		set_moving (netchanger.status_sch);
	end set_moving;


	procedure clear_moving (
		netchanger : in out type_netchanger)
	is begin
		clear_moving (netchanger.status_sch);
	end clear_moving;



	procedure set_proposed (
		netchanger : in out type_netchanger)
	is begin
		set_proposed (netchanger.status_sch);
	end set_proposed;



	procedure set_selected (
		netchanger : in out type_netchanger)
	is begin
		set_selected (netchanger.status_sch);
		set_selected (netchanger.status_brd);
	end set_selected;


	procedure clear_selected (
		netchanger : in out type_netchanger)
	is begin
		clear_selected (netchanger.status_sch);
		clear_selected (netchanger.status_brd);
	end clear_selected;



	procedure modify_status (
		netchanger	: in out type_netchanger;
		operation	: in type_status_operation)
	is begin
		modify_status (netchanger.status_sch, operation);
	end modify_status;



	procedure reset_status (
		netchanger	: in out type_netchanger)
	is begin
		reset_status (netchanger.status_sch);
		reset_status (netchanger.status_brd);
	end reset_status;








	function on_sheet_and_selected (
		netchanger	: in type_netchanger;
		sheet		: in type_sheet)
		return boolean
	is begin
		if get_sheet (netchanger) = sheet
		and is_selected (netchanger) then
			return true;
		else
			return false;
		end if;
	end on_sheet_and_selected;







	procedure copy_netchanger_with_offset (
		netchanger_in	: in type_netchanger;
		offset			: in type_object_position_relative;
		netchanger_out	: out type_netchanger)
	is
		procedure reset_board_position is
		begin
			netchanger_out.position_brd.place :=
				netchanger_default_place;
		end reset_board_position;


		-- Moves netchanger_out by the given offset:
		-- - sheet
		-- - x/y position
		procedure set_schematic_position is
			use et_sheets;
		begin
			-- sheet:
			move_netchanger (netchanger_out, get_sheet (offset));

			-- place (x/y):
			move_netchanger (netchanger_out, get_place (offset));
		end set_schematic_position;


	begin
		-- Make a full copy of the given netchanger:
		netchanger_out := netchanger_in;

		-- Now reset or clear some things of the copy.

		-- Reset board position to default:
		reset_board_position;

		-- Reset status flags:
		reset_status (netchanger_out.status_sch);
		reset_status (netchanger_out.status_brd);

		-- Set the schematic position:
		set_schematic_position;
	end copy_netchanger_with_offset;



end et_netchangers.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
