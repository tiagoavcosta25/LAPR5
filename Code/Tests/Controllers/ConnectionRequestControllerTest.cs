using System.Collections.Generic;
using Xunit;
using DDDSample1.Domain.Players;
using DDDNetCore.Domain.ConnectionRequests;
using DDDNetCore.Controllers;
using Moq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using DDDNetCore.Domain.ConnectionRequests.DTOS;
using DDDSample1.Domain.Shared;
using System;

namespace DDDNetCore.Tests.Controllers
{
    public class ConnectionRequestControllerTest
    {
        [Fact]
        public async Task ApproveRequest_ReturnsRequest()
        {
            // Arrange
            ApproveRequestDto dto = new ApproveRequestDto("request_pending", "middle_target_message");
            ConnectionRequestId cId = new ConnectionRequestId("12312322-4444-5555-6666-777888999000");

            var mockServ = new Mock<IConnectionRequestService>();
            mockServ.Setup(serv => serv.ApproveRequest(cId, dto))
                .ReturnsAsync(dto).Verifiable();
            var controller = new ConnectionRequestsController(mockServ.Object);

            // Act
            var result = await controller.ApproveRequest("12312322-4444-5555-6666-777888999000", dto);

            // Assert
            var actionValue = Assert.IsType<OkObjectResult>(result.Result);
            var returnValue = Assert.IsType<ApproveRequestDto>(actionValue.Value);
            mockServ.Verify();
            
            Assert.Equal(dto, returnValue);
        }

        [Fact]
        public async Task GetMiddleManRequests_ReturnsMiddleManRequests()
        {
            // Arrange
            IntroductionRequestDto dto = new IntroductionRequestDto("12312322-4444-5555-6666-777888999000", 
            "12312322-4444-5555-6666-777888999001", 
            "12312322-4444-5555-6666-777888999002",
            "12312322-4444-5555-6666-777888999003", "middle_target_message", "middle_target_message",
            "middle_target_message", "request_pending", 1, new List<string>{"tag1"});
            ConnectionRequestId cId = new ConnectionRequestId("12312322-4444-5555-6666-777888999000");
            string playerEmail = "test@email.com";
            List<IntroductionRequestDto> lst = new List<IntroductionRequestDto>{dto};

            var mockServ = new Mock<IConnectionRequestService>();
            mockServ.Setup(serv => serv.GetMiddleManRequests(playerEmail))
                .ReturnsAsync(lst).Verifiable();
            var controller = new ConnectionRequestsController(mockServ.Object);

            // Act
            var result = await controller.GetMiddleManRequests(playerEmail);

            // Assert
            var resultValue = Assert.IsType<List<IntroductionRequestDto>>(result.Value);
            mockServ.Verify();
            
            Assert.Equal(lst, resultValue);
        }

        [Fact]
        public async Task Create_ReturnsAIntroductionDto_WithIntroductionData()
        {
            // Arrange
            CreatingIntroductionRequestDto dto = new CreatingIntroductionRequestDto( 
            "12312322-4444-5555-6666-777888999001", 
            "12312322-4444-5555-6666-777888999002",
            "12312322-4444-5555-6666-777888999003", "middle_target_message", "middle_target_message",
            "middle_target_message", "request_pending", 1, new List<string>{"tag1"});
            IntroductionRequestDto dto2 = new IntroductionRequestDto("12312322-4444-5555-6666-777888999000", 
            "12312322-4444-5555-6666-777888999001", 
            "12312322-4444-5555-6666-777888999002",
            "12312322-4444-5555-6666-777888999003", "middle_target_message", "middle_target_message",
            "middle_target_message", "request_pending", 1, new List<string>{"tag1"});

            var mockServ = new Mock<IConnectionRequestService>();
            mockServ.Setup(serv => serv.AddIntAsync(dto))
                .ReturnsAsync(dto2).Verifiable();
            var controller = new ConnectionRequestsController(mockServ.Object);

            // Act
            var result = await controller.CreateIntr(dto);

            // Assert
            var actionResult = Assert.IsType<CreatedAtActionResult>(result.Result);
            
            var returnValue = Assert.IsType<IntroductionRequestDto>(actionResult.Value);
            mockServ.Verify();
            
            Assert.Equal(dto2, returnValue);
        }

        [Fact]
        public async Task Create_ReturnsBadRequestResult_WhenIntroductionDataNotValid()
        {
            // Arrange
            CreatingIntroductionRequestDto dto = new CreatingIntroductionRequestDto( 
            "12312322-4444-5555-6666-777888999001", 
            "12312322-4444-5555-6666-777888999002",
            "12312322-4444-5555-6666-777888999003", "middle_target_message", "middle_target_message",
            "middle_target_message", "request_pending", 1, new List<string>{"tag1"});
            IntroductionRequestDto dto2 = new IntroductionRequestDto("12312322-4444-5555-6666-777888999000", 
            "12312322-4444-5555-6666-777888999001", 
            "12312322-4444-5555-6666-777888999002",
            "12312322-4444-5555-6666-777888999003", "middle_target_message", "middle_target_message",
            "middle_target_message", "request_pending", 1, new List<string>{"tag1"});

            var mockServ = new Mock<IConnectionRequestService>();
            mockServ.Setup(serv => serv.AddIntAsync(dto))
                .ThrowsAsync(new BusinessRuleValidationException(""));
            var controller = new ConnectionRequestsController(mockServ.Object);

            // Act
            var result = await controller.CreateIntr(dto);

            // Assert
            var actionResult = Assert.IsType<BadRequestObjectResult>(result.Result);
            Assert.IsType<BadRequestObjectResult>(actionResult);
        }

        [Fact]
        public async Task GetAllUserPendingDirectRequests_ReturnsTargetPendingRequestDtoList()
        {
            // Arrange
            string email = "teste@gmail.com";
            TargetPendingRequestDto dto = new TargetDirectPendingRequestDto(email, "teste2@gmail.com", "message");
            List<TargetPendingRequestDto> lst = new() { dto };

            var mockServ = new Mock<IConnectionRequestService>();
            mockServ.Setup(serv => serv.GetAllUserPendingDirectRequestsAsync(email))
                .ReturnsAsync(lst).Verifiable();
            var controller = new ConnectionRequestsController(mockServ.Object);

            // Act
            var result = await controller.GetAllUserPendingDirectRequests(email);

            // Assert
            var returnValue = Assert.IsType<List<TargetPendingRequestDto>>(result.Value);
            mockServ.Verify();

            Assert.Equal(lst, returnValue);
        }


        [Fact]
        public async Task GetByEmails_ReturnsConnectionRequestDto()
        {
            // Arrange
            string email = "teste@gmail.com";
            string email2 = "teste2@gmail.com";
            ConnectionRequestDto dto = new DirectRequestDto(new Guid().ToString(), new Guid().ToString(), new Guid().ToString(), "message", "accepted", 3, new List<string> { "tag1" });


            var mockServ = new Mock<IConnectionRequestService>();
            mockServ.Setup(serv => serv.GetByEmailsAsync(email, email2))
                .ReturnsAsync(dto).Verifiable();
            var controller = new ConnectionRequestsController(mockServ.Object);

            // Act
            var result = await controller.GetByEmails(email, email2);

            // Assert
            //var actionResult = Assert.IsType<CreatedAtActionResult>(result.Result);

            var returnValue = Assert.IsType<DirectRequestDto>(result.Value);
            mockServ.Verify();

            Assert.Equal(dto, returnValue);
        }

        [Fact]
        public async Task AcceptRequest_ReturnsAcceptRequestDto()
        {
            // Arrange
            string email = "teste@gmail.com";
            string email2 = "teste2@gmail.com";
            AcceptRequestDto dto = new(email2, email, 3, new List<string> { "tag1"} );
            ConnectionRequestDto dto2 = new DirectRequestDto(new Guid().ToString(), new Guid().ToString(), new Guid().ToString(), "message", "accepted", 3, new List<string> { "tag1" });



            var mockServ = new Mock<IConnectionRequestService>();
            mockServ.Setup(serv => serv.AcceptRequest(dto))
                .ReturnsAsync(dto2).Verifiable();
            var controller = new ConnectionRequestsController(mockServ.Object);

            // Act
            var result = await controller.AcceptRequest(email, dto);

            var actionResult = Assert.IsType<OkObjectResult>(result.Result);
            
            
            var returnValue = Assert.IsType<AcceptRequestDto>(actionResult.Value);
            mockServ.Verify();

            Assert.Equal(dto, returnValue);
        }


        [Fact]
        public async Task AcceptRequest_WrongEmail()
        {
            // Arrange
            string email = "teste@gmail.com";
            string email2 = "teste2@gmail.com";
            AcceptRequestDto dto = new(email, email2, 3, new List<string> { "tag1" });
            ConnectionRequestDto dto2 = new DirectRequestDto(new Guid().ToString(), new Guid().ToString(), new Guid().ToString(), "message", "accepted", 3, new List<string> { "tag1" });



            var mockServ = new Mock<IConnectionRequestService>();
            mockServ.Setup(serv => serv.AcceptRequest(dto))
                .ReturnsAsync(dto2).Verifiable();
            var controller = new ConnectionRequestsController(mockServ.Object);

            // Act
            var result = await controller.AcceptRequest(email, dto);



            var actionResult = Assert.IsType<BadRequestResult>(result.Result);
            Assert.IsType<BadRequestResult>(actionResult);
        }
    }
}