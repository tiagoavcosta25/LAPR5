using System.Collections.Generic;
using Xunit;
using DDDSample1.Domain.Players;
using DDDSample1.Controllers;
using Moq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Controllers
{
    public class PlayerControllerTest
    {

        [Fact]
        public async Task Create_ReturnsAPlayerDto_WithPlayerData()
        {
            // Arrange
            CreatingPlayerDto dto = new CreatingPlayerDto("john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            /*Player obj = new Player(new PlayerName("john"), new PlayerName("test@email.com"), new PlayerPassword("123"), new PlayerPhoneNumber("987654321"),
                 new PlayerDateOfBirth(2001, 1, 17), new PlayerEmotionalStatus(OOC.joyful), new PlayerFacebook("www.facebook.com/john-doe"), 
                new PlayerLinkedIn("www.linkedin.com/john-doe"), new List<Tag>{new Tag("tag")});*/
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.AddAsync(dto))
                .ReturnsAsync(obj).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act
            var result = await controller.Create(dto);

            // Assert
            var actionResult = Assert.IsType<CreatedAtActionResult>(result.Result);
            
            var returnValue = Assert.IsType<PlayerDto>(actionResult.Value);
            mockServ.Verify();
            
            Assert.Equal(obj, returnValue);
        }

        [Fact]
        public async Task Create_ReturnsBadRequestResult_WhenPlayerDataNotValid()
        {
            // Arrange
            CreatingPlayerDto dto = new CreatingPlayerDto("john", "", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.AddAsync(dto))
                .ThrowsAsync(new BusinessRuleValidationException(""));
            var controller = new PlayersController(mockServ.Object, null);

            // Act
            var result = await controller.Create(dto);

            // Assert
            var actionResult = Assert.IsType<BadRequestObjectResult>(result.Result);
            Assert.IsType<BadRequestObjectResult>(actionResult);
        }

        [Fact]
        public async Task Update_ReturnsAPlayerDtoWithTheNewData()
        {
            // Arrange
            System.Guid id = new System.Guid();
            UpdatePlayerDto dto = new UpdatePlayerDto(id, "john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            PlayerDto obj = new PlayerDto(id, "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.UpdateAsync(dto))
            .ReturnsAsync(obj).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act
            var result = await controller.Update(id, dto);

            // Assert
            var actionResult = Assert.IsType<OkObjectResult>(result.Result);
            
            var returnValue = Assert.IsType<PlayerDto>(actionResult.Value);
            mockServ.Verify();
            
            Assert.Equal(obj, returnValue);
        }

        [Fact]
        public async Task Update_ReturnsBadRequestResult_WhenDifferentIds()
        {
            // Arrange
            System.Guid id = new System.Guid("b2e7111e-1a64-4ffe-8eba-a18fdc1c1e48");
            System.Guid id2 = new System.Guid("42c2df9d-709c-4b10-9d65-e0fbffb87293");
            UpdatePlayerDto dto = new UpdatePlayerDto(id, "john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            PlayerDto obj = new PlayerDto(id2, "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.UpdateAsync(dto))
            .ReturnsAsync(obj).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act
            var result = await controller.Update(id2, dto);

            // Assert
            var actionResult = Assert.IsType<BadRequestResult>(result.Result);
            Assert.IsType<BadRequestResult>(actionResult);
        }

        [Fact]
        public async Task Update_ReturnsNotFound_WhenPlayerNotFound()
        {
            // Arrange
            System.Guid id = new System.Guid();
            UpdatePlayerDto dto = new UpdatePlayerDto(id, "john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            PlayerDto obj = new PlayerDto(id, "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.UpdateAsync(dto))
            .ReturnsAsync((PlayerDto)null).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act
            var result = await controller.Update(id, dto);

            // Assert
            var actionResult = Assert.IsType<NotFoundResult>(result.Result);
            Assert.IsType<NotFoundResult>(actionResult);
        }

        [Fact]
        public async Task Update_ReturnsBadRequestResult_WhenPlayerDataNotValid()
        {
            // Arrange
            System.Guid id = new System.Guid();
            UpdatePlayerDto dto = new UpdatePlayerDto(id, "john", "test@email.com", "123", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            PlayerDto obj = new PlayerDto(id, "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.UpdateAsync(dto))
            .ThrowsAsync(new BusinessRuleValidationException(""));
            var controller = new PlayersController(mockServ.Object, null);

            // Act
            var result = await controller.Update(id, dto);

            // Assert
            var actionResult = Assert.IsType<BadRequestObjectResult>(result.Result);
            Assert.IsType<BadRequestObjectResult>(actionResult);
        }



        [Fact]
        public async Task ChangeEmotionalStatus_ReturnsChangeEmotionalStatusDto()
        {
            // Arrange
            string playerEmail = "teste@gmail.com";
            string emotionalStatus = "angry";
            ChangeEmotionalStatusDto dto = new(playerEmail, emotionalStatus);

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.ChangeEmotionalStatusAsync(dto))
            .ReturnsAsync(dto).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act
            var result = await controller.ChangeEmotionalStatus(playerEmail, dto);

            // Assert
            var actionResult = Assert.IsType<OkObjectResult>(result.Result);

            var returnValue = Assert.IsType<ChangeEmotionalStatusDto>(actionResult.Value);
            mockServ.Verify();

            Assert.Equal(dto, returnValue);
        }


        [Fact]
        public async Task ChangeEmotionalStatus_ReturnsBadRequestResult_WhenDifferentEmails()
        {
            // Arrange
            string playerEmail = "teste@gmail.com";
            string emotionalStatus = "angry";
            ChangeEmotionalStatusDto dto = new("wrong", emotionalStatus);

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.ChangeEmotionalStatusAsync(dto))
            .ReturnsAsync(dto).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act
            var result = await controller.ChangeEmotionalStatus(playerEmail, dto);

            // Assert
            var actionResult = Assert.IsType<BadRequestResult>(result.Result);
            Assert.IsType<BadRequestResult>(actionResult);
        }

        [Fact]
        public async Task ChangeEmotionalStatus_ReturnsBadRequestResult_WhenPlayerDataNotValid()
        {
            // Arrange
            string playerEmail = "teste@gmail.com";
            string emotionalStatus = "angry";
            ChangeEmotionalStatusDto dto = new(playerEmail, emotionalStatus);

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.ChangeEmotionalStatusAsync(dto))
            .ThrowsAsync(new BusinessRuleValidationException(""));
            var controller = new PlayersController(mockServ.Object, null);

            // Act
            var result = await controller.ChangeEmotionalStatus(playerEmail, dto);

            // Assert
            var actionResult = Assert.IsType<BadRequestObjectResult>(result.Result);
            Assert.IsType<BadRequestObjectResult>(actionResult);
        }

        [Fact]
        public async Task GetAllFiltered_ByEmail_ReturnsPlayersList()
        {
            // Arrange
            string filter = "email";
            string value = "teste@gmail.com";
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe",
            "www.linkedin.com/john-doe", new List<string> { "tag1" });
            ICollection<PlayerDto> lst = new List<PlayerDto> { obj };

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.GetByEmailAsync(value))
                .ReturnsAsync(obj).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act

            var result = await controller.GetAllFiltered(filter, value);

            // Assert
            var returnValue = Assert.IsType<List<PlayerDto>>(result.Value);
            mockServ.Verify();

            Assert.Equal(lst, returnValue);
        }

        [Fact]
        public async Task GetAllFiltered_ByName_ReturnsPlayersList()
        {
            // Arrange
            string filter = "name";
            string value = "john";
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe",
            "www.linkedin.com/john-doe", new List<string> { "tag1" });
            List<PlayerDto> lst = new List<PlayerDto> { obj };

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.GetByNameAsync(value))
                .ReturnsAsync(lst).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act

            var result = await controller.GetAllFiltered(filter, value);

            // Assert
            var returnValue = Assert.IsType<List<PlayerDto>>(result.Value);
            mockServ.Verify();

            Assert.Equal(lst, returnValue);
        }

        [Fact]
        public async Task GetAllFiltered_ByPhone_ReturnsPlayersList()
        {
            // Arrange
            string filter = "phone";
            string value = "987654321";
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe",
            "www.linkedin.com/john-doe", new List<string> { "tag1" });
            List<PlayerDto> lst = new List<PlayerDto> { obj };

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.GetByPhoneAsync(value))
                .ReturnsAsync(lst).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act

            var result = await controller.GetAllFiltered(filter, value);

            // Assert
            var returnValue = Assert.IsType<List<PlayerDto>>(result.Value);
            mockServ.Verify();

            Assert.Equal(lst, returnValue);
        }


        [Fact]
        public async Task GetAllFiltered_ByTag_ReturnsPlayersList()
        {
            // Arrange
            string filter = "tag";
            string value = "tag1";
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe",
            "www.linkedin.com/john-doe", new List<string> { "tag1" });
            List<PlayerDto> lst = new List<PlayerDto> { obj };

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.GetByTagAsync(value))
                .ReturnsAsync(lst).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act

            var result = await controller.GetAllFiltered(filter, value);

            // Assert
            var returnValue = Assert.IsType<List<PlayerDto>>(result.Value);
            mockServ.Verify();

            Assert.Equal(lst, returnValue);
        }


        [Fact]
        public async Task GetAllFiltered_BadRequest_NoFilter()
        {
            // Arrange
            string filter = "none";
            string value = "tag1";
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe",
            "www.linkedin.com/john-doe", new List<string> { "tag1" });
            List<PlayerDto> lst = new List<PlayerDto> { obj };

            var mockServ = new Mock<IPlayerService>();
            mockServ.Setup(serv => serv.GetByTagAsync(value))
                .ReturnsAsync(lst).Verifiable();
            var controller = new PlayersController(mockServ.Object, null);

            // Act

            var result = await controller.GetAllFiltered(filter, value);

            // Assert
            var actionResult = Assert.IsType<BadRequestResult>(result.Result);
            Assert.IsType<BadRequestResult>(actionResult);
        }



    }
}