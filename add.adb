with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;

-- Packages needed to generate pulse interrupts       
-- with Ada.Interrupts.Names;
-- with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------

    -----------------------------------------------------------------------
    ------------- constants 
    -----------------------------------------------------------------------
    LEER_POSICION_CABEZA_PRIORITY : Integer := 1;
    LEER_GIRO_VOLANTE_PRIORITY 	  : Integer := 2;
    LEER_DISTANCIA_PRIORITY 	  : Integer := 3;
    
    INTERVALO_LECTURA_POSICION_CABEZA : Time_Span := Milliseconds (400);
    INTERVALO_LECTURA_GIRO_VOLANTE    : Time_Span := Milliseconds (350);

    -----------------------------------------------------------------------
    ------------- declaration of tasks 
    -----------------------------------------------------------------------

    -- Aqui se declaran las tareas que forman el STR

    --task leerDistancia20_200ms is 
      --pragma priority (LEER_DISTANCIA_PRIORITY);
    --end leerDistancia20_200ms;
    
    task leerPosicionCabeza is 
      pragma priority (LEER_POSICION_CABEZA_PRIORITY);
    end leerPosicionCabeza;
    
    task leerGiroVolante is
      pragma priority (LEER_GIRO_VOLANTE_PRIORITY);
    end leerGiroVolante;

    -----------------------------------------------------------------------
    ------------- body of tasks 
    -----------------------------------------------------------------------

    -- Aqui se escriben los cuerpos de las tareas 

    --task body leerDistancia20_200ms is
      --Current_D : Distance_Samples_Type := 0;
      --Current_V : Speed_Samples_Type := 0;
    
    --begin
      --Starting_Notice ("Prueba sensor distancia");
      
      --for I in 1..20 loop
        --Reading_Distance (Current_D);
        --Display_Distance (Current_D);
      
        --Reading_Speed (Current_V);
        --Display_Speed (Current_V);
        
        --if (Current_V > 80) then
          --if (Current_D < 30) then 
            --Beep (5);  
          --elsif (Current_D < 60) then 
            --Beep (2);
          --end if;
        --end if;
        
        --New_Line;
        --delay until (Clock + To_time_Span(0.2));
        
      --end loop;
      
      --Finishing_Notice ("Prueba sensor distancia");
      
    --end leerDistancia20_200ms;
    
    
    -----------------------------------------------------------------------
    ------------- TAREA POSICION CABEZA 
    -----------------------------------------------------------------------
    
    task body leerPosicionCabeza is 
    
      Current_H   	 : HeadPosition_Samples_Type;
      Previous_H	 : HeadPosition_Samples_Type := (0, 0);
      Siguiente_Instante : Time;
      
    begin
    
      Siguiente_instante := Clock + INTERVALO_LECTURA_POSICION_CABEZA;
      Starting_Notice ("Prueba sensor cabeza");
      
      loop
      
        Reading_HeadPosition (Current_H);
        Display_HeadPosition_Sample (Current_H);
        
        if (Current_H(x) > 30 OR Current_H(x) < -30) then 
          if (Previous_H(x) > 30 OR Previous_H(x) < -30) then
            --Sintoma de somnolencia o distraccion
            Put (" --> DISTRACCION");
            New_Line;
          end if;
        end if;
        
        if (Current_H(y) > 30 OR Current_H(y) < -30) then 
          if (Previous_H(y) > 30 OR Previous_H(y) < -30) then
            --Sintoma de somnolencia o distraccion
            Put (" --> DISTRACCION");
            New_Line;
          end if;
        end if;
        
        Previous_H := Current_H;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_POSICION_CABEZA;
        
      end loop;
      
      Finishing_Notice ("Prueba sensor cabeza");
      
    end leerPosicionCabeza;
    
    
    -----------------------------------------------------------------------
    ------------- TAREA GIRO VOLANTE 
    -----------------------------------------------------------------------
    
    task body leerGiroVolante is
    
      Current_S     	 : Steering_Samples_Type;
      Previous_S    	 : Steering_Samples_Type := 0;
      Actual_Offset 	 : Steering_Samples_Type;
      Siguiente_Instante : Time;
      
    begin
      
      Siguiente_instante := Clock + INTERVALO_LECTURA_GIRO_VOLANTE;
      Starting_Notice ("Prueba sensor giro volante");
      
      loop
        
        Reading_Steering (Current_S);
        Display_Steering (Current_S);
        
        Actual_Offset := Current_S - Previous_S;
        
        if (Actual_Offset > 20 OR Actual_Offset < -20) then 
          --Sintoma de volantazo
          Put (" --> VOLANTAZO");
          New_Line;
        end if;
        
        Previous_S := Current_S;
        
        delay until Siguiente_Instante;
        Siguiente_Instante := Siguiente_Instante + INTERVALO_LECTURA_GIRO_VOLANTE;
        
      end loop;
      
      Finishing_Notice ("Prueba sensor giro volante");
      
    end leerGiroVolante;
   
    ----------------------------------------------------------------------
    ------------- procedure para probar los dispositivos 
    ----------------------------------------------------------------------
    procedure Prueba_Dispositivos; 

    Procedure Prueba_Dispositivos is
        Current_V : Speed_Samples_Type := 0;
        Current_H : HeadPosition_Samples_Type := (+2,-2);
        Current_D : Distance_Samples_Type := 0;
        Current_O : Eyes_Samples_Type := (70,70);
        Current_E : EEG_Samples_Type := (1,1,1,1,1,1,1,1,1,1);
        Current_S : Steering_Samples_Type := 0;
    begin
         --Starting_Notice ("Prueba_Dispositivo");

         --for I in 1..120 loop
         -- Prueba distancia
            --Reading_Distance (Current_D);
            --Display_Distance (Current_D);
            --if (Current_D < 40) then Light (On); 
            --                    else Light (Off); end if;

         -- Prueba velocidad
            --Reading_Speed (Current_V);
            --Display_Speed (Current_V);
            --if (Current_V > 110) then Beep (2); end if;

         -- Prueba volante
            --Reading_Steering (Current_S);
            --Display_Steering (Current_S);
            --if (Current_S > 30) OR (Current_S < -30) then Light (On);
            --                                         else Light (Off); end if;

         -- Prueba Posicion de la cabeza
            --Reading_HeadPosition (Current_H);
            --Display_HeadPosition_Sample (Current_H);
            --if (Current_H(x) > 30) then Beep (4); end if;

         -- Prueba ojos
            --Reading_EyesImage (Current_O);
            --Display_Eyes_Sample (Current_O);

         -- Prueba electroencefalograma
            --Reading_Sensors (Current_E);
            --Display_Electrodes_Sample (Current_E);
   
         --delay until (Clock + To_time_Span(0.1));
         --end loop;

         --Finishing_Notice ("Prueba_Dispositivo");
         null;
    end Prueba_Dispositivos;


begin
   null;
   --Starting_Notice ("Programa Principal");
   --Prueba_Dispositivos;
   --Finishing_Notice ("Programa Principal");
end add;



