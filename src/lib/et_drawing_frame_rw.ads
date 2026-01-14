------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                     DRAWING FRAME READ AND WRITE                         --
--                                                                          --
--                               S p e c                                    --
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


with et_logging;					use et_logging;
with et_schematic_coordinates;
with et_drawing_frame;				use et_drawing_frame;
with et_drawing_frame.schematic;	use et_drawing_frame.schematic;
with et_drawing_frame.board;		use et_drawing_frame.board;
with et_string_processing;			use et_string_processing;


package et_drawing_frame_rw is
	
	
	-- Creates and saves a frame in given file_name.
	procedure create_frame (
		file_name		: in pac_template_name.bounded_string;
		domain			: in type_domain;							   
		log_threshold	: in type_log_level);


	
	-- Saves the given schematic frame in file_name.
	procedure save_frame_schematic (
		frame			: in type_frame_schematic;
		file_name		: in pac_template_name.bounded_string;							 
		log_threshold	: in type_log_level);

	
	-- Saves the given board frame in file_name.
	procedure save_frame_board (
		frame			: in type_frame_pcb_pre;
		file_name		: in pac_template_name.bounded_string;							 
		log_threshold	: in type_log_level);



	-- Reads a frame template for the schematic as given by file_name and returns
	-- a schematic frame.
	function read_frame_schematic (
		file_name		: in pac_template_name.bounded_string;
		log_threshold	: in type_log_level)
		return type_frame_schematic;

	
	
	-- Reads a frame template for the board as given by file_name and returns
	-- a schematic frame.
	function read_frame_board (
		file_name		: in pac_template_name.bounded_string;
		log_threshold	: in type_log_level)
		return type_frame_pcb_pre;


	

	

	function to_position (
		line : in et_string_processing.type_fields_of_line; -- position x -100 y -150
		from : in et_string_processing.type_field_count_positive)
		return type_position;

	
end et_drawing_frame_rw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
