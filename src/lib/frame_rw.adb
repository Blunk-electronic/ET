------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              FRAME_RW                                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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


with ada.text_io;				use ada.text_io;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.maps;			use ada.strings.maps;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;

with ada.exceptions;

with et_coordinates;			use et_coordinates;
with et_general;				use et_general;
with et_text;
with et_frames;					use et_frames;
with et_string_processing;		use et_string_processing;
with general_rw;				use general_rw;

package body frame_rw is

	procedure write (
		frame			: in type_frame;
		file_name		: in pac_template_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is

		file_handle : ada.text_io.file_type;
	begin
		create (
			file 	=> file_handle,
			mode	=> out_file,
			name	=> to_string (file_name));

		set_output (file_handle);

		-- write a nice header
		put_line (comment_mark & " " & et_general.system_name & " drawing frame template");
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;



		-- write footer
		new_line;		
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " drawing frame template file end");
		new_line;
		
		reset_tab_depth;		


		set_output (standard_output);
		close (file_handle);

		exception when event: others =>
			log (text => ada.exceptions.exception_message (event));
			if is_open (file_handle) then
				close (file_handle);
			end if;
			raise;

	end write;
	
	procedure create_frame (
	-- Creates and saves a frame in given file_name.
		file_name		: in pac_template_name.bounded_string;
		domain			: in type_domain;							   
		log_threshold	: in et_string_processing.type_log_level) is

		frame : type_frame (domain);
	begin
		log (text => "creating frame " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "domain " & to_string (domain) & " ...", level => log_threshold);

		write (frame, file_name, log_threshold + 1);
		
		log_indentation_down;
	end create_frame;
	
	procedure save_frame (
	-- Saves the given frame in file_name.
		frame			: in type_frame;
		file_name		: in pac_template_name.bounded_string;
		log_threshold	: in et_string_processing.type_log_level) is
	begin
		log (text => "saving frame as " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "domain " & to_string (frame.domain) & " ...", level => log_threshold);

		write (frame, file_name, log_threshold + 1);
		
		log_indentation_down;
	end save_frame;

	function read_frame (
	-- Reads a frame from given file_name and returns a parameterized type_frame.
		file_name		: in pac_template_name.bounded_string;
		domain			: in type_domain;
		log_threshold	: in et_string_processing.type_log_level)
		return type_frame is

		frame : type_frame (SCHEMATIC);
	begin
		log (text => "reading frame " & to_string (file_name) & " ...", level => log_threshold);
		log_indentation_up;
		log (text => "domain " & to_string (domain) & " ...", level => log_threshold);

		log_indentation_down;

		return frame;
	end read_frame;
	
end frame_rw;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
