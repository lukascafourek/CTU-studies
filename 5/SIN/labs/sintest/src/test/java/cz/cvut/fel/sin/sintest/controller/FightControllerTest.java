package cz.cvut.fel.sin.sintest.controller;

import cz.cvut.fel.sin.sintest.exception.FieldMissingException;
import cz.cvut.fel.sin.sintest.exception.FightCannotHappenException;
import cz.cvut.fel.sin.sintest.exception.NotFoundException;
import cz.cvut.fel.sin.sintest.model.Group;
import cz.cvut.fel.sin.sintest.model.TypeOfFight;
import cz.cvut.fel.sin.sintest.service.FightService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.transaction.annotation.Transactional;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@Transactional
@SpringBootTest()
@AutoConfigureMockMvc(addFilters = false)
class FightControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private FightService fightService;

    @SpyBean
    private FightController fightController;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void fight_successfulPowerFight_returnsWinner() throws Exception {
        Group winner = new Group();
        winner.setId(1);
        TypeOfFight typeOfFight = TypeOfFight.POWER;

        when(fightService.fight(1, 2, typeOfFight)).thenReturn(winner);

        MvcResult result = mockMvc.perform(post("/fight")
                        .param("groupId1", "1")
                        .param("groupId2", "2")
                        .param("typeOfFight", "POWER"))
                .andExpect(status().isOk())
                .andReturn();

        String responseBody = result.getResponse().getContentAsString();
        assertEquals("Fight successful. Winner is 1", responseBody);
        assertEquals(200, result.getResponse().getStatus());
    }

    @Test
    void fight_successfulWillFight_returnsWinner() throws Exception {
        Group winner = new Group();
        winner.setId(1);
        TypeOfFight typeOfFight = TypeOfFight.WILL;

        when(fightService.fight(1, 2, typeOfFight)).thenReturn(winner);

        MvcResult result = mockMvc.perform(post("/fight")
                        .param("groupId1", "1")
                        .param("groupId2", "2")
                        .param("typeOfFight", "WILL"))
                .andExpect(status().isOk())
                .andReturn();

        String responseBody = result.getResponse().getContentAsString();
        assertEquals("Fight successful. Winner is 1", responseBody);
        assertEquals(200, result.getResponse().getStatus());
    }

    @Test
    void fight_successfulManaFight_returnsWinner() throws Exception {
        Group winner = new Group();
        winner.setId(1);
        TypeOfFight typeOfFight = TypeOfFight.MANA;

        when(fightService.fight(1, 2, typeOfFight)).thenReturn(winner);

        MvcResult result = mockMvc.perform(post("/fight")
                        .param("groupId1", "1")
                        .param("groupId2", "2")
                        .param("typeOfFight", "MANA"))
                .andExpect(status().isOk())
                .andReturn();

        String responseBody = result.getResponse().getContentAsString();
        assertEquals("Fight successful. Winner is 1", responseBody);
        assertEquals(200, result.getResponse().getStatus());
    }

    @Test
    void fight_failedFight_returnsBadRequest() throws Exception {
        when(fightService.fight(1, 2, TypeOfFight.POWER)).thenReturn(null);

        MvcResult result = mockMvc.perform(post("/fight")
                        .param("groupId1", "1")
                        .param("groupId2", "2")
                        .param("typeOfFight", "POWER"))
                .andExpect(status().isBadRequest())
                .andReturn();

        String responseBody = result.getResponse().getContentAsString();
        assertEquals("Failed to fight.", responseBody);
    }

    @Test
    void fight_missingField_throwsFieldMissingException() throws Exception {
        doThrow(new FieldMissingException("Field is missing")).when(fightService).fight(1, 2, TypeOfFight.POWER);

        MvcResult result = mockMvc.perform(post("/fight")
                        .param("groupId1", "1")
                        .param("groupId2", "2")
                        .param("typeOfFight", "POWER"))
                .andExpect(status().isBadRequest())
                .andReturn();

        String responseBody = result.getResponse().getContentAsString();
        assertEquals("Field is missing", responseBody);
    }

    @Test
    void fight_groupNotFound_throwsNotFoundException() throws Exception {
        doThrow(new NotFoundException("Group not found")).when(fightService).fight(1, 2, TypeOfFight.POWER);

        MvcResult result = mockMvc.perform(post("/fight")
                        .param("groupId1", "1")
                        .param("groupId2", "2")
                        .param("typeOfFight", "POWER"))
                .andExpect(status().isBadRequest())
                .andReturn();

        String responseBody = result.getResponse().getContentAsString();
        assertEquals("Group not found", responseBody);
    }

    @Test
    void fight_fightCannotHappen_throwsFightCannotHappenException() throws Exception {
        doThrow(new FightCannotHappenException("Fight cannot happen")).when(fightService).fight(1, 2, TypeOfFight.POWER);

        MvcResult result = mockMvc.perform(post("/fight")
                        .param("groupId1", "1")
                        .param("groupId2", "2")
                        .param("typeOfFight", "POWER"))
                .andExpect(status().isBadRequest())
                .andReturn();

        String responseBody = result.getResponse().getContentAsString();
        assertEquals("Fight cannot happen", responseBody);
    }
}
